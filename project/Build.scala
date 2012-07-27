import sbt._
import Keys._

object BuildSettings {

    val buildOrganization = "com.dadrox"
    val buildVersion = "0.1"
    val buildScalaVersion = "2.9.2"
    val revision = scala.sys.process.Process("git rev-parse --verify HEAD").lines(0)

    val buildSettings = Defaults.defaultSettings ++ Seq(
        organization := buildOrganization,
        version := buildVersion,
        scalaVersion := buildScalaVersion,
        resolvers := Resolvers.allResolvers,

        pomIncludeRepository := { _ => true },
        exportJars := true,
        libraryDependencies ++= Dependencies.common)
}

object Resolvers {
    val mavenDefault = DefaultMavenRepository
    val scalaTools = "New scala-tools repo" at "https://oss.sonatype.org/content/repositories/releases"
    val twitter = "Twitter repo" at "http://maven.twttr.com/"

    val allResolvers = Seq(scalaTools,twitter)
}

object Dependency {

    object V {
        val easymock = "3.1"
        val fictus = "0.2"
        val slf4j = "1.6.4"
        val twitterUtil = "5.2.0"
    }

    // Compile
    val grizzledSlf4j = "org.clapper" %% "grizzled-slf4j" % "0.6.9"
    val logbackClassic = "ch.qos.logback" % "logback-classic" % "1.0.0" % "container->default"
    val scalaIo = "com.github.scala-incubator.io" %% "scala-io" % "0.4.0"
    val slf4jApi = "org.slf4j" % "slf4j-api" % V.slf4j % "*->default"
    val snakeYaml = "org.yaml" % "snakeyaml" % "1.10"
    val twitterUtilCore = "com.twitter" % "util-core" % V.twitterUtil

    object Test {
        val easymock = "org.easymock" % "easymock" % V.easymock % "test->default"
        val fictus = "org.fictus" %% "fictus" % V.fictus % "test->default"
        val junit = "junit" % "junit" % "4.10" % "test->default"
        val junitInterface = "com.novocode" % "junit-interface" % "0.6" % "test->default"
        val logbackClassic = "ch.qos.logback" % "logback-classic" % "1.0.0" % "test->default"
    }
}

object Dependencies {
    import Dependency._

    val junit = Seq(Test.junit, Test.junitInterface)
    val slf4j = Seq(grizzledSlf4j, slf4jApi, Test.logbackClassic)

    val common = slf4j ++ junit :+ Test.easymock :+ twitterUtilCore :+ Test.fictus

    val tool = Seq(snakeYaml, scalaIo)
}

object CstBuild extends Build {
    import Resolvers._
    import BuildSettings._

    lazy val root = Project(
        id = "root",
        base = file("."),
        configurations = Configurations.default,
        settings = buildSettings,
        aggregate = Seq(tool))

    lazy val tool = Project(
        id = "tool",
        base = file("tool"),
        configurations = Configurations.default,
        settings = buildSettings ++ Seq(libraryDependencies ++= Dependencies.tool))
}
