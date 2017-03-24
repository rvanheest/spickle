package com.github.rvanheest.spickle.example.xml

import com.github.rvanheest.spickle.parser.xml.XmlParser._

import scala.xml.{ NamespaceBinding, TopScope, Utility }

trait Person {

  case class Number(number: String, addition: Option[String] = None)

  sealed abstract class Address(zipCode: String, city: String)
  case class RealAddress(street: String, number: Number, zipCode: String, city: String)
    extends Address(zipCode: String, city: String)
  case class FreepostAddress(number: String, zipCode: String, city: String)
    extends Address(zipCode: String, city: String)

  case class Person(name: String, age: Int, address: Address, mail: Option[String])
}

trait PersonXml { this: Person =>

  def parseNumber(name: String): XmlParser[Number] = {
    for {
      addition <- attributeId("addition").maybe
      number <- xmlToString(name)
    } yield Number(number, addition)
  }

  def parseRealAddress(name: String): XmlParser[Address] = {
    for {
      // no attributes here
      address <- branchNode(name) {
        for {
          street <- xmlToString("street")
          number <- parseNumber("number")
          zipCode <- xmlToString("zip-code")
          city <- xmlToString("city")
        } yield RealAddress(street, number, zipCode, city)
      }
    } yield address
  }

  def parseFreepostAddress(name: String): XmlParser[Address] = {
    for {
      // no attributes here
      address <- branchNode(name) {
        for {
          number <- xmlToString("freepost-number")
          zipCode <- xmlToString("zip-code")
          city <- xmlToString("city")
        } yield FreepostAddress(number, zipCode, city)
      }
    } yield address
  }

  def parseAddress(name: String): XmlParser[Address] = {
    parseRealAddress(name) <|> parseFreepostAddress(name)
  }

  def parsePerson: XmlParser[Person] = {
    implicit val xlinkNamespace = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    for {
      age <- attribute("age")(_.toInt)
      _ <- namespaceAttribute("age").map(_.toInt)
      p <- branchNode("person") {
        for {
          pName <- xmlToString("name")
          address <- parseAddress("address")
          mail <- xmlToString("mail").maybe
        } yield Person(pName, age, address, mail)
      }
    } yield p
  }
}

object PersonXmlExample extends App with Person with PersonXml {

  val xml1 = <person age="24" xlink:age="24">
    <name>Richard van Heest</name>
    <address>
      <street>Prins Bernhardlaan</street>
      <number>116</number>
      <zip-code>3241TA</zip-code>
      <city>Middelharnis</city>
    </address>
    <mail>richard.v.heest@gmail.com</mail>
  </person>

  val xml2 = <person age="24" xlink:age="24">
    <name>Richard van Heest</name>
    <address>
      <street>Prins Bernhardlaan</street>
      <number addition="a">116</number>
      <zip-code>3241TA</zip-code>
      <city>Middelharnis</city>
    </address>
    <mail>richard.v.heest@gmail.com</mail>
  </person>

  val xml3 = <person age="24" xlink:age="24">
    <name>Richard van Heest</name>
    <address>
      <freepost-number>12345</freepost-number>
      <zip-code>3241TA</zip-code>
      <city>Middelharnis</city>
    </address>
  </person>

  println(parsePerson.run(Utility.trim(xml1)))
  println(parsePerson.run(Utility.trim(xml2)))
  println(parsePerson.run(Utility.trim(xml3)))
}
