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
    val ModuleScope = SubKey("scope", Some(Modules))
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
        extends Key {
}

class Build(fileSource: FileSource = new FileProvider()) {

    def parse(): Response[BuildDefinition] = {
        fileSource.asInputStream("./build.yaml") match {
            case Succeeds(is) =>
                val yaml = new Yml(is)

                for {
                    settings <- yaml.settings
                    //                    paths <- Succeeds(yaml.paths)
                    //                    versions <- yaml.versions
                    modules <- yaml.modules
                } yield new BuildDefinition(settings, modules)

            case wtf => Fails(Failure.NotFound, "Unable to find the build.yaml")
        }
    }
}

class Yml(is: InputStream) {
    import org.yaml.snakeyaml.Yaml
    import java.io.InputStream

    val yaml = treeified(is)

    println(yaml)

    private def missing(key: Key) = Fails(Failure.NotFound, key + " is a required element of the build, bitch")

    private def requiredRootKey(key: RootKey): Response[Mapped] = yaml match {
        case mapped: Mapped => Succeeds(mapped)
        case wtf            => missing(key)
    }

    private def getRequired(mapped: Mapped, key: Key) = mapped.getLeaf(key.name) match {
        case Some(value) => Succeeds(value)
        case None        => missing(key)
    }
    private def optional(mapped: Mapped, key: Key) = mapped.getLeaf(key.name)

    def modules(): Response[List[Module]] = {

        yaml match {
            case mapped: Mapped =>
                mapped.getNode(Key.Modules.name) match {
                    case Some(Node(modules)) =>
                        val res = modules.map {
                            case mod: Mapped =>
                                for {
                                    name <- getRequired(mod, Key.ModuleName)
                                    path <- getRequired(mod, Key.ModulePath)
                                    libraries <- {
                                        mod.get(Key.ModuleLibraries.name) match {
                                            case Some(libs: Node) => toLibraries(libs)
                                            case Some(wtf)        => Fails(Failure.Malformed, "Libraries must be a list. E.g. [ *lib1, *lib2 ], Found: " + wtf)
                                            case None             => Succeeds(Nil)
                                        }
                                    }
                                    modules <- {
                                        mod.getNode(Key.ModuleModules.name) match {
                                            case Some(Node(mods)) =>
                                                Succeeds(mods.flatMap {
                                                    case Leaf(value) => List(value)
                                                    case _           => Nil
                                                })
                                            case None => Succeeds(Nil)
                                        }
                                    }
                                } yield Module(name, path, modules, libraries, optional(mod, Key.ModuleScope))
                            case wtf => Fails(Failure.Malformed, "Malformed modules")
                        }
                        Response.collect(res)
                    case wtf => missing(Key.Modules)
                }
            case wtf => missing(Key.Modules)
        }
    }

    private def toLibraries(node: Node): Response[List[Library]] = {

        println("lib node=" + node)

        val libs: List[Response[List[Library]]] = node.items.map {
            case Node(List(Leaf(org), Leaf(artifactName), Leaf(version), Leaf(scope))) =>
                println("lib w/ scope")
                Succeeds(List(Library(org, artifactName, version, Some(scope))))
            case Node(List(Leaf(org), Leaf(artifactName), Leaf(version))) =>
                println("lib w/o scope")
                Succeeds(List(Library(org, artifactName, version)))
            case n: Node => toLibraries(n)
            case Mapped(map) =>
                if (map.size > 1) Fails(Failure.Malformed, "Malformed library, must be a single entry, found=" + map)
                else {
                    val (k, v) = map.head
                    val lib = k match {
                        case Node(List(Leaf(org), Leaf(artifactName), Leaf(version), Leaf(scope))) =>
                            println("lib w/ scope")
                            Succeeds(Library(org, artifactName, version, Some(scope)))
                        case Node(List(Leaf(org), Leaf(artifactName), Leaf(version))) =>
                            println("lib w/o scope")
                            Succeeds(Library(org, artifactName, version))
                        case wtf     => Fails(Failure.Malformed, "Malformed library, must be a lib, found=" + wtf)
                    }
                    val scope = v match {
                        case Leaf(scope) => Succeeds(scope)
                        case wtf         => Fails(Failure.Malformed, "Malformed library, scope must be a string, found=" + wtf)
                    }
                    for {
                        lib <- lib
                        scope <- scope
                    } yield List(lib.copy(scope = Some(scope)))
                }
            case wtf =>
                println("WTF... library=" + wtf)
                Fails(Failure.Malformed, "Libraries must be a list. E.g. [ *lib1, *lib2 ], Found: " + wtf)
        }
        println("libs=" + libs)
        libs.find(_.isFailure) match {
            case None => Succeeds(libs.flatMap(x => x.getSuccess))
            case Some(failure) =>
                val responseFailure = failure.getFail
                Fails(responseFailure.failureType, responseFailure.msg, responseFailure.entireResponse)
        }
    }

    def settings(): Response[Settings] = {
        val settings = for {
            settings <- yaml match {
                case map: Mapped => map.getMapped(Key.Settings.name)
                case _           => None
            }
            name <- settings.getLeaf(Key.SettingsOrg.name)
            org <- settings.getLeaf(Key.SettingsOrg.name)
            version <- settings.getLeaf(Key.SettingsVersion.name)
        } yield Settings(version, org)

        settings match {
            case Some(settings) => Succeeds(settings)
            case None           => Fails(Failure.NotFound, "Awww")
        }
    }

    def paths(): Paths = {

        // HACKHACK
        Paths()
    }

    private def treeified(is: InputStream): Tree = {
        val yaml = new Yaml().load(is)

        println("raw=" + yaml)

        import scala.collection.JavaConverters.mapAsScalaMapConverter
        import scala.collection.JavaConverters.asScalaBufferConverter

        def objectToXml(o: Any): Tree = o match {
            case list: java.util.List[_]  => Node(list.asScala.map(objectToXml).toList)
            case map: java.util.Map[_, _] => Mapped((map.asScala.map { case (k, v) => objectToXml(k) -> objectToXml(v) }).toMap)
            case s: String                => Leaf(s)
            case i: java.lang.Integer     => Leaf(i.toString)
            case d: java.lang.Double      => Leaf(d.toString)
            case b: java.lang.Boolean     => Leaf(b.toString)
            case null                     => Leaf("CRAP")
        }

        objectToXml(yaml)
    }
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
    modules: List[Module] = Nil)

case class Settings(
    version: String,
    org: String)

case class LibraryToken(name: String, parsedThing: String)

case class Library(
    org: String,
    artifiactName: String,
    version: String,
    scope: Option[String] = None)

case class Module(
    name: String,
    path: String,
    modules: List[String] = Nil,
    libraries: List[Library] = Nil,
    scope: Option[String] = None)

case class Versions(versions: Map[String, String] = Map())

sealed trait Tree
case class Leaf(value: String) extends Tree {
    override def toString: String = value.toString
}
case class Node(items: List[Tree]) extends Tree {
    override def toString: String = "Node(" + items.mkString(", ") + ")"
}
case class Mapped(items: Map[Tree, Tree]) extends Tree {
    override def toString: String = "Mapped(" + items.mkString(", ") + ")"

    def getLeaf(key: String) = items.get(Leaf(key)) match {
        case Some(Leaf(value)) => Some(value)
        case _                 => None
    }
    def getMap(key: String) = items.get(Leaf(key)) match {
        case Some(Mapped(value)) => Some(value)
        case _                   => None
    }
    def getMapped(key: String) = items.get(Leaf(key)) match {
        case Some(Mapped(value)) => Some(Mapped(value))
        case _                   => None
    }
    def getNode(key: String) = items.get(Leaf(key)) match {
        case Some(Node(value)) => Some(Node(value))
        case _                 => None
    }
    def get(key: String) = items.get(Leaf(key))
}
























