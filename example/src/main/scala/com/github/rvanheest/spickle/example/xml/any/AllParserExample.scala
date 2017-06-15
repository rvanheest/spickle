package com.github.rvanheest.spickle.example.xml.any

import java.nio.file.{ Files, Paths }

import scala.xml.{ Utility, XML }

object AllParserExample extends App {

  val path = Paths.get(getClass.getResource("/all/person1.xml").toURI)
  val xml = Utility.trim(XML.loadFile(path.toFile))

  println(PersonParser.parsePersons.parse(xml))
}
