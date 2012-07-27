package com.dadrox

import org.junit.Test
import org.fictus.Fictus
import java.io.ByteArrayOutputStream
import java.io.PrintWriter
import java.io.ByteArrayInputStream
import java.io.InputStream
import org.yaml.snakeyaml.Yaml

class BuildDefinitionTest extends Fictus {

    val path = "./build.yaml"
    val version = "1.0"
    val org = "com.dadrox"

    val fileSource = mock[FileSource]
    val unit = new Build(fileSource)

    @Test
    def invalidYaml {
        fileSource.asInputStream(*) --> Fails(Failure.NotFound, "")

        test(unit.parse()) mustMatch {
            case Fails(Failure.NotFound, _, _) =>
        }
    }

    @Test
    def getRequiredSettings_present {
        expectStream("""|settings:
                        |  version: 1.0
                        |  org: com.dadrox
                        |
                        |modules:
                        | - name: foo
                        |   path: path""")

        test(unit.parse()) mustEqual Succeeds(BuildDefinition(Settings(version, org), modules = List(Module("foo", "path"))))
    }

    @Test
    def libraries_ok {
        expectStream("""|settings:
                        |  org: com.dadrox
                        |  version: 1.0
                        |
                        |versions:
                        |    &finagleVersion 5.1.0
                        |
                        |libraries:
                        |    ? &finagle-core       [ org.twitter, finagle-core, *finagleVersion ]
                        |    ? &finagle-http       [ org.twitter, finagle-http, *finagleVersion ]
                        |    ? &finagle-memcached  [ org.twitter, finagle-memcached, *finagleVersion ]
                        |    ? &finagle            [ *finagle-core, *finagle-http, *finagle-memcached ]
                        |
                        |    ? &servlet-api        [ javax-servlet,servlet-api,2.5,provided ]
                        |
                        |    ? &junit              [ org.junit,junit,4.8,test ]
                        |    ? &easymock           [ org.easymock,easymock,3.0,test ]
                        |    ? &test-stuff         [ *easymock, *junit ]
                        |
                        |modules:
                        | - name: foo
                        |   path: path
                        |   libraries: [ *finagle, *test-stuff ]
                        |
                        | - name: bar
                        |   path: barpath
                        |   modules: [ foo ]
                        |   libraries: [ *servlet-api ]
                        |""")

        val foo = Module("foo", "path", libraries = List(
            Library("org.twitter", "finagle-core", "5.1.0"),
            Library("org.twitter", "finagle-http", "5.1.0"),
            Library("org.twitter", "finagle-memcached", "5.1.0"),
            Library("org.easymock", "easymock", "3.0", Some("test")),
            Library("org.junit", "junit", "4.8", Some("test"))))
        val bar = Module("bar", "barpath", List(foo), libraries = List(
            Library("javax-servlet", "servlet-api", "2.5", Some("provided"))))

        test(unit.parse()) mustEqual Succeeds(
            BuildDefinition(Settings(version, org), List(foo, bar)))
    }

    @Test
    def realistic {
        expectStream("""|settings:
                        |  org: com.twc.cst
                        |  version: 1.0
                        |
                        |versions:
                        |    &finagleVersion 5.1.0
                        |
                        |libraries:
                        |    ? &finagle-core       [ org.twitter, finagle-core, *finagleVersion ]
                        |    ? &finagle-http       [ org.twitter, finagle-http, *finagleVersion ]
                        |    ? &finagle-memcached  [ org.twitter, finagle-memcached, *finagleVersion ]
                        |    ? &finagle            [ *finagle-core, *finagle-http, *finagle-memcached ]
                        |
                        |    ? &jodaTime           [ joda-time, joda-time, 1.6.2 ]
                        |
                        |    ? &servlet-api        [ javax-servlet,servlet-api,2.5,provided ]
                        |
                        |    ? &junit              [ org.junit,junit,4.8,test ]
                        |    ? &easymock           [ org.easymock,easymock,3.0,test ]
                        |    ? &test-stuff         [ *easymock, *junit ]
                        |
                        |    ? &common             [ *jodaTime, *test-stuff ]
                        |
                        |modules:
                        | - name: utility
                        |   path: core/common/utility
                        |   modules: [ test-utility ]
                        |   libraries: [ *common ]
                        |
                        | - name: test-utility
                        |   path: core/common/test-utility
                        |   libraries: [ *junit:provided->default, *easymock:provided, *jodaTime ]
                        |   scope: test
                        |
                        | - name: model
                        |   path: core/model
                        |   modules: [ utility, test-utility ]
                        |   libraries: [ *common ]
                        |
                        | - name: http
                        |   path: core/common/http
                        |   modules: [ model, utility, test-utility ]
                        |   libraries: [ *common, *finagle ]
                        |""")

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

        test(unit.parse()) mustEqual Succeeds(
            BuildDefinition(Settings(version, "com.twc.cst"), List(testUtility, utility, model, http)))
    }

    @Test
    def getRequiredSettings_missing_version {
        expectStream("""|settings:
                        |  org: com.dadrox""")

        test(unit.parse()) mustMatch {
            case Fails(Failure.NotFound, _, _) =>
        }
    }

    @Test
    def requiredSettings_missing_org {
        expectStream("""|settings:
                        |  version: 1.0""")

        test(unit.parse()) mustMatch {
            case Fails(Failure.NotFound, _, _) =>
        }
    }

    @Test
    def yaml_testing {
        print("""|- a
                 |- b
                 |- c""")
        print("1.0")
        print("1")
        print("yes")
        print("YES")
        print("true")
        print("0")
        print("yes and no")
        print("""|- 1.0
                |- 1
                |- yes
                |- foo""")
        print("""|settings:
                 | version: 1.0
                 | org: com.dadrox
                 | foo: bar
                 | baz: fuz""")
    }

    private def print(yaml: String) {
        val a = new Yaml().load(new ByteArrayInputStream(yaml.stripMargin.getBytes))
        println(yaml + ", " + a + ", " + a.getClass)

    }

    private def expectStream(contents: String) =
        fileSource.asInputStream(*) --> Succeeds(new ByteArrayInputStream(contents.stripMargin.getBytes))

    val definition = """
paths:
      # defaults
      src-root: src
      src: \{src-root}/main
      resources: \{src}/resources
      resources: \{src-root}/test
      test-resources: \{src}/resources
      output: target
      generated-src-dir: \{output}/generated-src
      output-classes: \{output}/classes
      output-test-classes: \{output}/test-classes

settings:
      # required
      version: 1.0
      org: com.dadrox
      resolver: local(ENV_3RDPARTY_DIR)

versions:
    - &finagle: 5.1.0

    - &finagleVersion 5.1.0

libraries:
    finagle-core: org.twitter:finagle-core:*finagleVersion
    finagle-http: org.twitter:finagle-http:*finagleVersion
    finagle-memcached: org.twitter:finagle-memcached:*finagleVersion
    servlet-api: javax-servlet:servlet-api:2.5:provided
    junit: org.junit:junit:4.8:test->default
    easymock: org.easymock:easymock:3.0:test->default

projects:
   - name: cst
     aggregate: [api]
     plugins: [ war, scaladoc ]

   - name: the-other-project
     aggregate: utility, test-utility[test], model
     plugins: [ war, scaladoc ]

modules:
   - name: api
     path: ./api
     modules: utility, test-utility, model
     libraries: [ servlet-api ]
     artifactName: cst-api # overrides name for package
     version: 2.0 # overrides global version, not recommended
     org: overrideable?

   - name: utility
     path: ./utility
     modules: test-utility
     libraries: [junit, easymock, springx]

   - name: test-utility
     path: ./test-utility
     modules:
     libraries: [ junit->compile, easymock-compile ]

   - name: model
     path: ./model
     modules:
            - utility
            - test-utility
     libraries:

        """
}