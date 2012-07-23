package com.dadrox

import java.io.File
import java.util.{ Map => Jmap }
import scala.collection.JavaConverters.mapAsScalaMapConverter
import java.io.InputStream

object Key {
    val Settings = Key("settings", None)
    val Libraries = Key("libraries", None)
    val Project = Key("project", None)
    val Modules = Key("module", None)

    val SettingsOrg = Key("org", Some(Settings))
    val SettingsVersion = Key("version", Some(Settings))

    val ModuleVersion = Key("version", Some(Modules))
}
case class Key(name: String, parent: Option[Key]) {
    def path(): List[String] = (name :: (parent match {
        case Some(p) => p.path
        case None    => Nil
    })).reverse
}

class Build(fileSource: FileSource = new FileProvider()) {

    def parse(): Response[BuildDefinition] = {
        fileSource.asInputStream("./build.yaml") match {
            case Succeeds(is) =>
                val yaml = new Yml(is)

                //                val settings = for {
                //                    settings <- yaml.getRequired(Key.Settings)
                //                    org <- yaml.getRequired(Key.SettingsOrg)
                //                    version <-yaml.getRequired(Key.SettingsVersion)
                //                } yield Settings(version, org)

                val settings: Response[Settings] = yaml.get(Key.Settings) match {
                    case None => Fails(Failure.NotFound, "Settings is a required element of the build, bitch")
                    case Some(settings) => yaml.get(Key.SettingsOrg) match {
                        case None => Fails(Failure.NotFound, "Settings:org is a required element of the build, bitch")
                        case Some(org) => yaml.get(Key.SettingsVersion) match {
                            case None          => Fails(Failure.MissingVersion, "Settings:version is a required element of the build, bitch")
                            case Some(version) => Succeeds(Settings(version, org))
                        }
                    }
                }

                println("wtf=" + settings)

                settings flatMap { settings =>
                    Succeeds(new BuildDefinition(settings))
                }

            case wtf => Fails(Failure.NotFound, "Unable to find the build.yaml")
        }
    }
}

class Yml(is: InputStream) {
    import org.yaml.snakeyaml.Yaml
    import java.io.InputStream

    type Ymap = Jmap[String, Any]

    val yaml = new Yaml().load(is).asInstanceOf[Ymap].asScala

    println(yaml)

    def getRequired(key: Key): Response[String] = get(key) match {
        case Some(value) => Succeeds(value)
        case None        => Fails(Failure.NotFound, key.path.mkString(":") + " is a required element of the build, bitch")
    }

    def get(key: Key): Option[String] = {
        val path = key.path

        println("path=" + path)

        def get(names: List[String], current: Map[String, Any]): Option[String] = {
            names match {
                case lastOne :: Nil =>
                    println("HERE")
                    val xx = current.get(lastOne)
                    println("HERE " + xx.getClass)
                    xx.asInstanceOf[Option[String]]
                case Nil            => None
                case more =>
                    current.get(names.head) match {
                        case Some(theMap) => get(names.tail, theMap.asInstanceOf[Ymap].asScala.toMap)
                        case None         => None
                    }

            }
        }
        get(path, yaml.toMap)
    }
}

object LibraryScope extends Enum {
    sealed case class EnumVal private[LibraryScope](key: String) extends Value

    val Test = EnumVal("test")
    val Provided = EnumVal("provided")
    val All = EnumVal("")
    val Compile = EnumVal("compile")
    val Runtime = EnumVal("runtime")
}

case class BuildDefinition(
    settings: Settings)

case class Settings(
    version: String,
    org: String)

case class LibraryToken(name: String, parsedThing: String)
case class Library(name: String, org: String, artifiactName: String, version: String, scope: Option[LibraryScope.EnumVal])


