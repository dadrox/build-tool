package com.dadrox

import org.junit.Test
import java.io.File
import org.fictus.Fictus
import org.junit.Before
import org.junit.BeforeClass

object RunTest {

    val joda = Library("joda-time", "joda-time", "1.6.2")
    val junit = Library("org.junit", "junit", "4.8", Some("test"))
    val easymock = Library("org.easymock", "easymock", "3.0", Some("test"))
    val commonLibs = List(joda, easymock, junit)

    val testUtility = Module("test-utility", "core/common/test-utility",
        libraries = List(
            junit.copy(scope = Some("provided->default")),
            easymock.copy(scope = Some("provided")),
            joda),
        scope = Some("test"))

    val utility = Module("utility", "core/common/utility", List(testUtility), libraries = commonLibs)

    val model = Module("model", "core/model", List(utility, testUtility), libraries = commonLibs)

    val http = Module("http", "core/common/http", List(model, utility, testUtility),
        libraries = commonLibs ++ List(
            Library("org.twitter", "finagle-core", "5.1.0"),
            Library("org.twitter", "finagle-http", "5.1.0"),
            Library("org.twitter", "finagle-memcached", "5.1.0")))

    val foo = Module("foo", "core/common/foo")

    val build = BuildDefinition(Settings("1.0", "com.twc.cst"), List(testUtility, utility, model, http))

    val workingDirectory = new File("/home/cwood/git/twc/4th-x-platform")

    @BeforeClass
    def before {
        println("Working directory = " + workingDirectory)
        println("Build = " + build)
    }
}
class RunTest extends Fictus {
    import RunTest._
    val unit = new Lifecycle(build, workingDirectory = workingDirectory)

    @Test
    def run {
        unit.validate mustEqual Succeeds(true)
    }

    @Test
    def tasks {
        println(Task.tasks(Task.Clean))
        println(Task.tasks(Task.Compile))
        println(Task.tasks(Task.Initialize))
        println(Task.tasks(Task.IntegrationTest))
        println(Task.tasks(Task.Package))
        println(Task.tasks(Task.Test))
        println(Task.tasks(Task.Validate))
    }

    @Test
    def clean {
        unit.clean mustEqual Succeeds(true)
    }
}