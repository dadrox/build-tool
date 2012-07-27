package com.dadrox

import java.io.File

class Lifecycle(
        buildDefinition: BuildDefinition,
        fileSource: FileSource = new FileProvider,
        workingDirectory: File = new File(".")) {

    def validate(): Response[Boolean] = {
        validModulePaths(buildDefinition.modules)
    }

    def validModulePaths(modules: List[Module]): Response[Boolean] = {

        buildDefinition.modules.foreach { module =>
            val resp = validModulePath(module)
            if (resp.isFailure)
                return resp
        }
        Succeeds(true)

        //        (buildDefinition.modules.map { module =>
        //            validModulePath(module)
        //        })
        //        .find(_.isFailure) match {
        //            case Some(failure) => failure
        //            case None          => Succeeds(true)
        //        }
    }

    def validModulePath(module: Module) = {
        val dir = moduleDir(module)

        //        file match {
        //            case file if (!file.exists) => Fails(Failure.Foo, "path=" + path + " does not exist!")
        //            case file if (!file.canRead)  => Fails(Failure.Foo, "path=" + path + " is not writeable!")
        //            case file if (!file.isDirectory)      => Fails(Failure.Foo, "path=" + path + " is not a directory!")
        //            case win                                  => Succeeds(true)
        //        }

        lazy val broken = "Module [" + module.name + "] path [" + module.path + "] "

        if (!dir.exists) Fails(Failure.Foo, "does not exist!")
        else if (!dir.canRead) Fails(Failure.Foo, "is not writeable!")
        else if (!dir.isDirectory) Fails(Failure.Foo, "is not a directory!")
        else Succeeds(true)
    }

    def init() = {

    }

    def clean() = {
        val results = buildDefinition.modules.map { module =>
            val target = moduleTargetDir(module)
            print(target)
            target.delete match {
                case true  => Succeeds(true)
                case false => Fails(Failure.Foo, "Unable to clean Module[" + module.name + "]")
            }
        }
        results.find(_.isFailure) match {
            case Some(failure) => failure
            case None          => Succeeds(true)
        }
    }

    private def moduleDir(module: Module) = fileSource.file(new File(workingDirectory, module.path).getCanonicalPath)
    private def moduleTargetDir(module: Module) = fileSource.file(new File(workingDirectory, module.path + "/target/").getCanonicalPath)

    private def print(file: File) {
        println(file + ", " + (if (file.canRead) "r" else "-") + (if (file.canWrite) "w" else "-") + (if (file.canExecute) "x" else "-"))
    }
}

sealed case class Task(
    name: String,
    parent: Option[Task])

object Task {

    val Validate = Task("validate", None)
    val Initialize = Task("init", Some(Validate))
    val Clean = Task("clean", Some(Validate))
    val Compile = Task("compile", Some(Validate))
    val Test = Task("test", Some(Compile))
    val IntegrationTest = Task("it", Some(Compile))
    val Package = Task("package", Some(Compile))

    def tasks(task: Task): List[Task] = {
        (task :: {
            task.parent match {
                case Some(task) => tasks(task)
                case None       => Nil
            }
        }).reverse
    }
}