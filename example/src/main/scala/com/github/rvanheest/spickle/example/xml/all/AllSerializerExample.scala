package com.github.rvanheest.spickle.example.xml.all

import com.github.rvanheest.spickle.example.xml.all.PersonSerializer.Person

import scala.util.Success
import scala.xml.PrettyPrinter

object AllSerializerExample extends App {

  val person1 = Person("Jan", "Hus", 25)
  val person2 = Person("Martin", "Luther", 50)
  val person3 = Person("John", "Calvin", 40)
  val persons = List(person1, person2, person3)
  val nums = List(1, 2, 3, 4, 5)

  val Success(xmls) = PersonSerializer.serializePersons.serialize((persons, nums), Seq.empty)
  for (xml <- xmls) {
    println(new PrettyPrinter(160, 2).format(xml))
  }
}
