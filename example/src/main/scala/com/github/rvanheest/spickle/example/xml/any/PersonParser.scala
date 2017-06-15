package com.github.rvanheest.spickle.example.xml.any

import com.github.rvanheest.spickle.parser.Parser
import com.github.rvanheest.spickle.parser.xml.XmlParser.XmlParser
import com.github.rvanheest.spickle.parser.xml.XmlParser._

object PersonParser {
  case class Person(firstName: String, lastName: String, age: Int)

  def parseFirstName: XmlParser[String] = stringNode("firstname")

  def parseLastName: XmlParser[String] = stringNode("lastname").map(_.toUpperCase)

  def parseAge: XmlParser[Int] = stringNode("age").map(_.toInt)

  def parsePersonContent: XmlParser[(String, String, Int)] = {
    all(parseFirstName, parseLastName, parseAge)(mandatory, mandatory, mandatory)
  }

  def parsePerson: XmlParser[Person] = {
    branchNode("person")(parsePersonContent.map(Person.tupled))
  }

  def parsePersons: XmlParser[Seq[Person]] = branchNode("persons")(parsePerson.many)
}
