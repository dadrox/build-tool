package com.dadrox

import java.io.File

class Build(fileSource: FileSource = new FileProvider()) {

    def parse(): Response[BuildDefinition] = {
        val file = new File("./build.yaml")
        file match {
            case yaml if(file.exists) => Succeeds(new BuildDefinition())
            case wtf => Fails(Failure.NotFound, "Unable to find the build.yaml")
        }
    }
}

case class BuildDefinition(
                ) {

}