package com.github.rvanheest.spickle.example.xml.person

object PersonParserExample extends App with Person with PersonXml with PersonParser {

  println(parsePerson.parse(xml1))
  println(parsePerson.parse(xml2))
  println(parsePerson.parse(xml3))
}
