package com.dadrox

object Defaults {
    val srcRoot = "src"
    val resources = srcRoot + "/resources"
    val testSrc = srcRoot + "/test"
    val testResources = testSrc + "/resources"
    val output = "target"
    val outputClasses = output + "/classes"
    val outputTestClasses = output + "/test-classes"
}

case class Paths(
    srcRoot: String = Defaults.srcRoot,
    resources: String = Defaults.resources,
    testSrc: String = Defaults.testSrc,
    testResources: String = Defaults.testResources,
    output: String = Defaults.output,
    outputClasses: String = Defaults.outputClasses,
    outputTestClasses: String = Defaults.outputTestClasses)