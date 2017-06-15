package com.github.rvanheest.spickle.example.xml.all

import java.nio.file.Paths

import scala.util.Success
import scala.xml.{ Utility, XML }

object AllPickleExample extends App {

  val path = Paths.get(getClass.getResource("/all/person1.xml").toURI)
  val xml = Utility.trim(XML.loadFile(path.toFile))

  val parse @ (Success(persons), rest) = PersonPickle.picklePersons.parse(xml)
  println(parse)

  val pickle @ Success(pickledXml) = PersonPickle.picklePersons.pickle(persons, rest)
  println(pickle)

  val parse2 = PersonPickle.picklePersons.parse(pickledXml)
  println(parse2)
}
