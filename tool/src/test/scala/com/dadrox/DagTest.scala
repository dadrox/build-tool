package com.dadrox

import org.fictus.Fictus
import org.junit.Test

class DagTest extends Fictus {
    @Test
    def works {
        val in = Map(
            "d" -> List("c"),
            "b" -> List("a"),
            "f" -> List("b", "a", "e"),
            "e" -> List("d", "b"),
            "a" -> List(),
            "c" -> List("b", "a"))
        Dag.dag(in) mustEqual Succeeds(List("a", "b", "c", "d", "e", "f"))
    }

    @Test
    def cycle {
        val in = Map(
            "b" -> List("c"),
            "a" -> List(),
            "c" -> List("b"))
        Dag.dag(in) mustMatch {
            case Fails(Failure.Malformed, _, _) =>
        }
    }
}