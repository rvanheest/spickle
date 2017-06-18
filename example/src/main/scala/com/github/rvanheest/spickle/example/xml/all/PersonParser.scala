package com.github.rvanheest.spickle.example.xml.all

import com.github.rvanheest.spickle.parser.xml.XmlParser.{ XmlParser, _ }

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

  def parseSomeNumbers: XmlParser[Seq[Int]] = {
    stringNode("somenumbers")
      .map(_.split(" ").map(_.toInt))
  }

  def parsePersons: XmlParser[(Seq[Person], Seq[Int])] = branchNode("persons") {
    for {
      persons <- parsePerson.many
      numbers <- parseSomeNumbers
    } yield (persons, numbers)
  }
}
