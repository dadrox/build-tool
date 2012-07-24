package com.dadrox

import java.io.File
import java.util.{ Map => Jmap }
import scala.collection.JavaConverters.mapAsScalaMapConverter
import java.io.InputStream

object Key {
    val Settings = RootKey("settings")
    val Libraries = RootKey("libraries")
    val Project = RootKey("project")
    val Modules = RootKey("modules")
    val Paths = RootKey("paths")
    val Versions = RootKey("versions")

    val SettingsOrg = SubKey("org", Some(Settings))
    val SettingsVersion = SubKey("version", Some(Settings))

    val ModuleName = SubKey("name", Some(Modules))
    val ModulePath = SubKey("path", Some(Modules))
    val ModuleLibraries = SubKey("libraries", Some(Modules))
    val ModuleModules = SubKey("modules", Some(Modules))
}
trait Key {
    val name: String
    val parent: Option[Key]

    def path(): List[String] = (name :: (parent match {
        case Some(p) => p.path
        case None    => Nil
    })).reverse

    override def toString = path.mkString(":")
}

case class RootKey(
    override val name: String)
        extends Key {
    override val parent = None
}
case class SubKey(
    override val name: String,
    override val parent: Some[Key])
        extends Key

class Build(fileSource: FileSource = new FileProvider()) {

    def parse(): Response[BuildDefinition] = {
        fileSource.asInputStream("./build.yaml") match {
            case Succeeds(is) =>
                val yaml = new Yml(is)

                for {
                    settings <- yaml.settings
                    paths <- Succeeds(yaml.paths)
                    versions <- yaml.versions
                    modules <- yaml.modules
                } yield new BuildDefinition(settings, paths, versions, modules)

            case wtf => Fails(Failure.NotFound, "Unable to find the build.yaml")
        }
    }
}

class Yml(is: InputStream) {
    import org.yaml.snakeyaml.Yaml
    import java.io.InputStream

    type Ymap = Jmap[String, Any]

    val yaml = scalafied(is).asInstanceOf[Map[String, Any]]

    println(yaml)

    private def missing(key: Key) = Fails(Failure.NotFound, key.path.mkString(":") + " is a required element of the build, bitch")

    def modules(): Response[List[Module]] = {
        val xx = yaml.get(Key.Modules.name) match {
            case None => Succeeds(Nil)
            case Some(modules) => modules match {
                case modules: List[_] => Response.collect {
                    modules.map {
                        case module: Map[String, _] => toModule(module)
                        case wtf                    => Fails(Failure.Malformed, "Malformed module")
                    }
                }
                case wtf => Succeeds(Nil)
            }
        }
        println("XX=" + xx)
        xx
    }

    private def toModule(module: Map[String, Any]): Response[Module] = {
        module.get(Key.ModuleName.name) match {
            case Some(name: String) => module.get(Key.ModulePath.name) match {
                case Some(path: String) =>
                    val libraries = module.get(Key.ModuleLibraries.name) match {
                        case Some(Nil)                => Succeeds(Nil)
                        case Some(list: List[String]) => Succeeds(list)
                        case Some(s: String)          => Fails(Failure.Malformed, "In module " + name + ": libraries must be a list. E.g. [m1, m2]")
                        case wtf                      => Succeeds(Nil)
                    }
                    val modules = module.get(Key.ModuleModules.name) match {
                        case Some(Nil)                => Succeeds(Nil)
                        case Some(list: List[String]) => Succeeds(list)
                        case Some(s: String)          => Fails(Failure.Malformed, "In module " + name + ": modules must be a list. E.g. [m1, m2]")
                        case wtf                      => Succeeds(Nil)
                    }
                    for {
                        mods <- modules
                        libs <- libraries
                    } yield Module(name, path, mods, libs)

                case nothing => missing(Key.ModulePath)
            }
            case nothing => missing(Key.ModuleName)
        }
    }

    def versions(): Response[Versions] = {
        yaml.get(Key.Versions.name) match {
            case None => Succeeds(Versions())
            case Some(versions) => versions match {
                case versions: Map[String, String] => Succeeds(Versions(versions.asInstanceOf[Map[String, String]]))
                case wtf                           => Fails(Failure.Malformed, "versions must be key value pairs")
            }
        }
    }

    def settings(): Response[Settings] = {
        val settings = for {
            settings <- yaml.get(Key.Settings.name) match {
                case Some(settings) => Some(settings.asInstanceOf[Map[String, String]])
                case None           => None
            }
            name <- settings.get(Key.SettingsOrg.name)
            org <- settings.get(Key.SettingsOrg.name)
            version <- settings.get(Key.SettingsVersion.name)
        } yield Settings(version, org)

        settings match {
            case Some(settings) => Succeeds(settings)
            case None           => Fails(Failure.NotFound, "Awww")
        }

        //        yaml.get(Key.Settings.name) match {
        //            case None => missing(Key.Settings)
        //            case Some(settings) => settings match {
        //                case settings: Map[String, String] =>
        //                    settings.get(Key.SettingsOrg.name) match {
        //                        case None => missing(Key.SettingsOrg)
        //                        case Some(org) => settings.get(Key.SettingsVersion.name) match {
        //                            case Some(version) => Succeeds(Settings(version, org))
        //                            case None          => missing(Key.SettingsVersion)
        //                        }
        //                    }
        //                case wtf => missing(Key.Settings)
        //            }
        //        }
    }

    def paths(): Paths = {
        yaml.get(Key.Paths.name) match {
            case Some(paths) => paths match {
                case paths: Map[String, String] =>
                    Defaults(
                        srcRoot = paths.get("src-root"),
                        resources = paths.get("resources"),
                        testSrc = paths.get("test-src"),
                        testResources = paths.get("test-resources"),
                        output = paths.get("output"),
                        outputClasses = paths.get("output-classes"),
                        outputTestClasses = paths.get("output-test-classes"))
                case wtf => Paths()
            }
            case None => Paths()
        }

        // HACKHACK
        Paths()
    }

    private def scalafied(is: InputStream) = {
        val yaml = new Yaml().load(is)
        import scala.collection.JavaConverters.mapAsScalaMapConverter
        import scala.collection.JavaConverters.asScalaBufferConverter
        def objectToXml(o: Any): Any = o match {
            case list: java.util.List[_]  => list.asScala.map(objectToXml).toList
            //            case list: java.util.ArrayList[_] => list.asScala.map(objectToXml).toList
            case map: java.util.Map[_, _] => (map.asScala.map { case (k, v) => objectToXml(k) -> objectToXml(v) }).toMap
            //            case amap: java.util.LinkedHashMap[_, _] => (amap.asScala.map{case (k,v)=> objectToXml(k) -> objectToXml(v)}).toMap
            case s: String                => s
            case i: java.lang.Integer     => i.toString
            case d: java.lang.Double      => d.toString
            case b: java.lang.Boolean     => b.toString
            case null                     => "CRAP"
        }

        objectToXml(yaml)
    }

    //    def getRequired(key: Key): Response[String] = get(key) match {
    //        case Some(value) => Succeeds(value)
    //        case None        => Fails(Failure.NotFound, key.path.mkString(":") + " is a required element of the build, bitch")
    //    }

    //    def get(key: RootKey)
    //    def get(key: Key): Option[String] = {
    //        val path = key.path
    //
    //        println("path=" + path)
    //
    //        def get(names: List[String], current: Map[String, Any]): Option[String] = {
    //            names match {
    //                case lastOne :: Nil =>
    //                    println("HERE")
    //                    val xx = current.get(lastOne)
    //                    println("HERE " + xx.getClass)
    //                    xx.asInstanceOf[Option[String]]
    //                case Nil => None
    //                case more =>
    //                    current.get(names.head) match {
    //                        case Some(theMap) => get(names.tail, theMap.asInstanceOf[Ymap].asScala.toMap)
    //                        case None         => None
    //                    }
    //
    //            }
    //        }
    //        get(path, yaml.toMap)
    //    }
}

object LibraryScope extends Enum {
    sealed case class EnumVal private[LibraryScope] (key: String) extends Value

    val Test = EnumVal("test")
    val Provided = EnumVal("provided")
    val All = EnumVal("")
    val Compile = EnumVal("compile")
    val Runtime = EnumVal("runtime")
}

case class BuildDefinition(
    settings: Settings,
    paths: Paths = Paths(),
    versions: Versions = Versions(),
    modules: List[Module] = Nil)

case class Settings(
    version: String,
    org: String)

case class LibraryToken(name: String, parsedThing: String)

case class Library(
    name: String,
    org: String,
    artifiactName: String,
    version: String,
    scope: Option[LibraryScope.EnumVal])

case class Module(
    name: String,
    path: String,
    modules: List[String] = Nil,
    libraries: List[String] = Nil)

case class Versions(versions: Map[String, String] = Map())



























