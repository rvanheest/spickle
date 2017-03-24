package com.github.rvanheest.spickle.example.xml.person

object PersonParserExample extends App with Person with PersonXml with PersonParser {

  println(parsePerson.run(xml1))
  println(parsePerson.run(xml2))
  println(parsePerson.run(xml3))
}
