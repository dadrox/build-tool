package com.dadrox

import scala.annotation.tailrec

object Dag {
    def dag(items: Map[String, List[String]]): Response[List[String]] = {
        val (roots, nonroots) = items.partition(_._2.size == 0)

        @tailrec
        def topologicalSort(acc: List[String], remaining: Map[String, List[String]]): Response[List[String]] = {

            val (matches, theRest) = remaining.partition(_._2.forall(acc.contains))

            matches.keys.toList match {
                case Nil => theRest.keys.toList match {
                    case Nil  => Succeeds(acc)
                    case some => Fails(Failure.Malformed, "Cycle detected among " + some)
                }
                case some => topologicalSort(acc ++ some, theRest)
            }
        }

        topologicalSort(roots.keys.toList, nonroots)
    }
}