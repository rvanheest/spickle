package com.github.rvanheest.spickle.example.xml.person

import scala.util.Success

object PersonPickleExample extends App with Person with PersonXml with PersonPickle {

  val (Success(person1), rest1) = picklePerson.unpickle(xml1)
  val Success(person1Xml) = picklePerson.pickle(person1, Seq.empty)

  println(person1)
  println(rest1)
  println(person1Xml)
  println(xml1.toString() == person1Xml.toString())
  println

  val (Success(person2), rest2) = picklePerson.unpickle(xml2)
  val Success(person2Xml) = picklePerson.pickle(person2, Seq.empty)

  println(person2)
  println(rest2)
  println(person2Xml)
  println(xml2.toString() == person2Xml.toString())
  println

  val (Success(person3), rest3) = picklePerson.unpickle(xml3)
  val Success(person3Xml) = picklePerson.pickle(person3, Seq.empty)

  println(person3)
  println(rest3)
  println(person3Xml)
  println(xml3.toString() == person3Xml.toString())
}
