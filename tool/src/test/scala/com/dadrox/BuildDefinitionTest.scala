package com.dadrox

import org.junit.Test
import org.fictus.Fictus
import java.io.ByteArrayOutputStream
import java.io.PrintWriter
import java.io.ByteArrayInputStream
import java.io.InputStream

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
        expectStream("""
            |settings:
            |  version: "1.0"
            |  org: com.dadrox""")

        test(unit.parse()) mustEqual Succeeds(BuildDefinition(Settings(version, org)))
    }

    @Test
    def getRequiredSettings_missing_version {
        expectStream("""settings:
                        |  org: com.dadrox""")

        test(unit.parse()) mustMatch {
            case Fails(Failure.MissingVersion, _, _) =>
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

    private def expectStream(contents: String) =
        fileSource.asInputStream(path) --> Succeeds(new ByteArrayInputStream(contents.stripMargin.getBytes))

    val definition = """
settings:
      # defaults
      src-root: src
      src: \{src-root}/main
      resources: \{src}/resources
      test-src: \{src-root}/test
      test-resources: \{src}/resources
      output: target
      generated-src-dir: \{output}/generated-src
      output-classes: \{output}/classes
      output-test-classes: \{output}/test-classes

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
      test:
            junit:            org.junit:junit:4.8
            easymock:   org.easymock:easymock:3.0

project:
   -name: cst
      aggregate: api
      plugins: [ war, scaladoc ]

   -name: the-other-project
      aggregate: utility, test-utility[test], model
      plugins: [ war, scaladoc ]

module:
   -name: api
      path: ./api
      modules: utility, test-utility, model
      libraries: servlet-api
      artifactName: cst-api # overrides name for package
      version: 2.0 # overrides global version, not recommended
      org: overrideable?

   -name: utility
      path: ./utility
      modules: test-utility
      libraries: [ junit, easymock, springx ]

   -name: test-utility
      path: ./test-utility
      modules:
      libraries: [ junit:compile, easymock:compile ]

   -name: model
      path: ./model
      modules:
            -utility
            -test-utility
      libraries:

   -name:
      path:
      modules:
      libraries:
        """

}