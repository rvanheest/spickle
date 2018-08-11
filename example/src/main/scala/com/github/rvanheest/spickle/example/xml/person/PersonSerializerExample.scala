package com.github.rvanheest.spickle.example.xml.person

import com.github.rvanheest.spickle.serializer.xml.XmlSerializer._
import scala.util.Success
import scala.xml.PrettyPrinter

object PersonSerializerExample extends App with Person with PersonXml with PersonSerializer {

  val Success(person1Xml) = serializePerson.serialize(object1)
  for (xml <- person1Xml) {
    println(new PrettyPrinter(160, 4).format(xml))
  }
  println(xml1.toString() == person1Xml.toString())
  println

  val Success(person2Xml) = serializePerson.serialize(object2)
  for (xml <- person2Xml) {
    println(new PrettyPrinter(160, 4).format(xml))
  }
  println(xml2.toString() == person2Xml.toString())
  println

  val Success(person3Xml) = serializePerson.serialize(object3)
  for (xml <- person3Xml) {
    println(new PrettyPrinter(160, 4).format(xml))
  }
  println(xml3.toString() == person3Xml.toString())
}
