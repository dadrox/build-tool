package com.dadrox

import org.fictus.Fictus
import org.junit.Test

class DagTest extends Fictus {
    @Test
    def foo {
        val in = Map(
            "d" -> List("c"),
            "b" -> List("a"),
            "f" -> List("b", "a", "e"),
            "e" -> List("d", "b"),
            "a" -> List(),
            "c" -> List("b", "a"))
        println(in)
        Dag.dag(in) mustEqual List("a", "b", "c", "d", "e", "f")
    }
}