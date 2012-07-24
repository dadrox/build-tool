package com.dadrox

import org.yaml.snakeyaml.Yaml
import java.io.ByteArrayInputStream
import java.util.ArrayList
import java.util.LinkedHashMap
import scala.collection.JavaConverters.mapAsScalaMapConverter
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.xml.Node
import scala.annotation.tailrec


object Yxml {

    def toXml(yaml: String) = {
        val a = new Yaml().load(new ByteArrayInputStream(yaml.stripMargin.getBytes))

        def objectToXml(o: Any):Any = o match {
            case list: java.util.List[_] => list.asScala.map(objectToXml).toList
//            case list: java.util.ArrayList[_] => list.asScala.map(objectToXml).toList
            case map: java.util.Map[_, _] => (map.asScala.map{case (k,v)=> objectToXml(k) -> objectToXml(v)}).toMap
//            case amap: java.util.LinkedHashMap[_, _] => (amap.asScala.map{case (k,v)=> objectToXml(k) -> objectToXml(v)}).toMap
            case s: String => s
            case i: java.lang.Integer => i.toString
            case d: java.lang.Double => d.toString
            case b: java.lang.Boolean => b.toString
            case null => "CRAP"
        }

        objectToXml(a)
    }


}