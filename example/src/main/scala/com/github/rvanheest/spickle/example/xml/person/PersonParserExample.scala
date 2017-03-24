package com.github.rvanheest.spickle.example.xml.person

import scala.xml.Utility

object PersonParserExample extends App with Person with PersonXml with PersonParser {

  println(parsePerson.run(Utility.trim(xml1)))
  println(parsePerson.run(Utility.trim(xml2)))
  println(parsePerson.run(Utility.trim(xml3)))
}
