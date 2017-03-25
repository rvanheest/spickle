package com.github.rvanheest.spickle.example.xml.person

import com.github.rvanheest.spickle.parser.xml.XmlParser._

import scala.xml.{ NamespaceBinding, TopScope }

trait PersonParser {
  this: Person =>

  def parseNumber(name: String): XmlParser[Number] = {
    for {
      addition <- attribute("addition").maybe
      number <- nodeToString(name)
    } yield Number(number, addition)
  }

  def parseRealAddress(name: String): XmlParser[Address] = {
    for {
      // no attributes here
      address <- branchNode(name) {
        for {
          street <- nodeToString("street")
          number <- parseNumber("number")
          zipCode <- nodeToString("zip-code")
          city <- nodeToString("city")
        } yield RealAddress(street, number, zipCode, city)
      }
    } yield address
  }

  def parseFreepostAddress(name: String): XmlParser[Address] = {
    for {
      // no attributes here
      address <- branchNode(name) {
        for {
          number <- nodeToString("freepost-number")
          zipCode <- nodeToString("zip-code")
          city <- nodeToString("city")
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
      age <- attribute("age").toInt
      _ <- namespaceAttribute("age").toInt
      p <- branchNode("person") {
        for {
          pName <- nodeToString("name")
          address <- parseAddress("address")
          mail <- nodeToString("mail").maybe
        } yield Person(pName, age, address, mail)
      }
    } yield p
  }
}
