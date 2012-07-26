package com.dadrox

import scala.annotation.tailrec

object Dag {
    def dag(items: Map[String, List[String]]): List[String] = {
        val (roots, nonroots) = items.partition(_._2.size == 0)

        @tailrec
        def topologicalSort(acc: List[String], remaining: Map[String, List[String]]): List[String] = {

            val (matches, theRest) = remaining.partition(_._2.forall(acc.contains))

            matches.keys.toList match {
                case Nil => theRest.keys.toList match {
                    case Nil  => acc
                    case some => throw new RuntimeException("dag is jacked " + some)
                }
                case some => topologicalSort(acc ++ some, theRest)

            }
        }

        topologicalSort(roots.keys.toList, nonroots)
    }
}