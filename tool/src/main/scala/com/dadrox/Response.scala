package com.dadrox

sealed case class ResponseFailure private[dadrox] (failureType: Failure.EnumVal,
                                                     msg: String,
                                                     entireResponse: String = "") {
    override def toString() =
        "Failure -- failureType = " + failureType + ", msg = " + msg + ", entireResponse: " + entireResponse + " --ResponseFailure--"
}

object Response {
    def collect[A](lr: List[Response[A]]): Response[List[A]] = {
        lr match {
            case Nil => Succeeds(List())
            case responses => responses.find(_.isFailure) match {
                case None => Succeeds(responses.map(x => x.getSuccess))
                case Some(failure) =>
                    val responseFailure = failure.getFail
                    Fails(responseFailure.failureType, responseFailure.msg, responseFailure.entireResponse)
            }
        }
    }
}

sealed abstract class Response[+A](wrappedEither: Either[ResponseFailure, A]) extends Serializable {
    def isFailure: Boolean
    def isSuccess: Boolean

    def getSuccess(): A = wrappedEither.right.get
    def getFail(): ResponseFailure = wrappedEither.left.get

    def fold[B](fail: Fails => B, succeed: A => B) = wrappedEither match {
        case Left(failure) => fail(Fails(failure.failureType, failure.msg, failure.entireResponse))
        case Right(obj)    => succeed(obj)
    }

    def map[B](f: A => B): Response[B] = this match {
        case Succeeds(obj)                           => Succeeds(f(obj))
        case Fails(failureType, msg, entireResponse) => Fails(failureType, msg, entireResponse)
    }

    def flatMap[B](f: A => Response[B]): Response[B] = this match {
        case Succeeds(obj)                           => f(obj)
        case Fails(failureType, msg, entireResponse) => Fails(failureType, msg, entireResponse)
    }

    def onSuccess[B](nextResponse: A => Response[B]) = flatMap(nextResponse)

    def isFailureOfType(failureType: Failure.EnumVal): Boolean = {
        wrappedEither match {
            case Left(actualFailureType) => actualFailureType.failureType == failureType
            case Right(_)                => false
        }
    }
}

final case class Fails(failureType: Failure.EnumVal, msg: String, entireResponse: String = "") extends Response[Nothing](Left(ResponseFailure(failureType, msg, entireResponse))) {
    override val isFailure = true
    override val isSuccess = false
}

final case class Succeeds[+A](obj: A) extends Response[A](Right(obj)) {
    override val isFailure = false
    override val isSuccess = true
}