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
        fileSource.asInputStream(path) --> Fails(Failure.NotFound, "")

        test(unit.parse()) mustMatch {
            case Fails(Failure.NotFound, _, _) =>
        }
    }

    @Test
    def getRequiredSettings_present {
        expectStream("""|settings:
                        |  version: 1.0
                        |  org: com.dadrox""")

        test(unit.parse()) mustEqual Succeeds(BuildDefinition(Settings(version, org)))
    }

    @Test
    def versions_ok {
        expectStream("""|settings:
                        |  version: 1.0
                        |  org: com.dadrox
                        |
                        |versions:
                        |  fictus: 9.2
                        |  finagle: 5.3
                        |""")

        test(unit.parse()) mustEqual Succeeds(BuildDefinition(Settings(version, org), Paths(), Versions(Map(
                        "fictus" -> "9.2",
                        "finagle" -> "5.3"))))
    }

    @Test
    def modules_ok {
        expectStream("""|settings:
                        |  version: 1.0
                        |  org: com.dadrox
                        |
                        |modules:
                        | - name: foo
                        |   path: path
                        |
                        | - name: bar
                        |   path: thepath
                        |   modules: [blah]
                        |   libraries: [a,b,c]
                        |""")

                        test(unit.parse()) mustEqual Succeeds(BuildDefinition(Settings(version, org), Paths(), Versions(), List(
                                        Module("foo", "path", Nil, Nil),
                                        Module("bar", "thepath", List("blah"), List("a","b","c")))))
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
    def requiredSettings_missing_ {

    }

    @Test
    def getDefaultPaths {

    }

    @Test
    def getOverriddenPaths {

    }

    @Test
    def yaml {
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

    @Test
    def fullYaml {
        val xx = Yxml.toXml(definition)
        println(xx.asInstanceOf[Map[Any, Any]].mkString("\n"))
    }

    private def print(yaml: String) {
        //        val a = new Yaml().load(new ByteArrayInputStream(yaml.stripMargin.getBytes))
        val a = Yxml.toXml(yaml)
        println(yaml + ", " + a + ", " + a.getClass)

    }

    private def expectStream(contents: String) =
        fileSource.asInputStream(path) --> Succeeds(new ByteArrayInputStream(contents.stripMargin.getBytes))

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


libraries:
      finagle: org.whatev::1.0 # implicitly all

      spring1:
      spring2:
      spring3:

      springx: [ spring1, spring2, spring3 ]

      compile:
      provided:         javax:servlet-api:2.5
      runtime:
      test->default:
        org.junit:junit:4.8
      test:
            junit:            org.junit:junit:4.8:test->default
            easymock:   org.easymock:easymock:3.0:test->default

projects:
   - name: cst
     aggregate: [api]
     plugins: [ war, scaladoc ]

   - name: the-other-project
     aggregate: utility, test-utility[test], model
     plugins: [ war, scaladoc ]

cst:

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