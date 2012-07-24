package com.dadrox

object Failure extends Enum {
    sealed case class EnumVal private[Failure] () extends Value

    val Foo = EnumVal()
    val MissingVersion = EnumVal()
    val NotFound = EnumVal()
    val Malformed = EnumVal()
}