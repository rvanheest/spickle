package com.github.rvanheest.spickle.example.xml.person

import scala.util.Success
import scala.xml.PrettyPrinter

object PersonPickleExample extends App with Person with PersonXml with PersonPickle {

  val (Success(person1), rest1) = picklePerson.parse(xml1)
  val Success(person1Xml) = picklePerson.serialize(person1, Seq.empty)

  println(person1)
  println(rest1)
  for (xml <- person1Xml) {
    println(new PrettyPrinter(160, 4).format(xml))
  }
  println(xml1.toString() == person1Xml.toString())
  println

  val (Success(person2), rest2) = picklePerson.parse(xml2)
  val Success(person2Xml) = picklePerson.serialize(person2, Seq.empty)

  println(person2)
  println(rest2)
  for (xml <- person2Xml) {
    println(new PrettyPrinter(160, 4).format(xml))
  }
  println(xml2.toString() == person2Xml.toString())
  println

  val (Success(person3), rest3) = picklePerson.parse(xml3)
  val Success(person3Xml) = picklePerson.serialize(person3, Seq.empty)

  println(person3)
  println(rest3)
  for (xml <- person3Xml) {
    println(new PrettyPrinter(160, 4).format(xml))
  }
  println(xml3.toString() == person3Xml.toString())
}
