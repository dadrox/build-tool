package com.dadrox

/*
/src/main/scala
         /resources
    /test/scala
         /resources
    /it/scala
       /resources
/target/classes
       /resources
       /test-classes



/src/scala
    /resources
/test/scala
     /resources
/it/scala
   /resources

/target/classes
       /resources
       /test-classes
 */


object New {
    val src = "src/main"
    val resources = "src/main/resources"

}

object Defaults {
    val srcRoot = "src"
    val src = srcRoot + "/main"
    val resources = src + "/resources"
    val testSrc = srcRoot + "/test"
    val testResources = testSrc + "/resources"
    val output = "target"
    val outputClasses = output + "/classes"
    val outputTestClasses = output + "/test-classes"

    def apply(
        srcRoot: Option[String] = None,
        src: Option[String] = None,
        resources: Option[String] = None,
        testSrc: Option[String] = None,
        testResources: Option[String] = None,
        output: Option[String] = None,
        outputClasses: Option[String] = None,
        outputTestClasses: Option[String] = None) = {
        new Paths(
            srcRoot = srcRoot.getOrElse(Defaults.srcRoot),
            src = src.getOrElse(Defaults.src),
            resources = resources.getOrElse(Defaults.resources),
            testSrc = testSrc.getOrElse(Defaults.testSrc),
            testResources = testResources.getOrElse(Defaults.testResources),
            output = output.getOrElse(Defaults.output),
            outputClasses = outputClasses.getOrElse(Defaults.outputClasses),
            outputTestClasses = outputTestClasses.getOrElse(Defaults.outputTestClasses))
    }
}

case class Paths(
    srcRoot: String = Defaults.srcRoot,
    src: String = Defaults.src,
    resources: String = Defaults.resources,
    testSrc: String = Defaults.testSrc,
    testResources: String = Defaults.testResources,
    output: String = Defaults.output,
    outputClasses: String = Defaults.outputClasses,
    outputTestClasses: String = Defaults.outputTestClasses)