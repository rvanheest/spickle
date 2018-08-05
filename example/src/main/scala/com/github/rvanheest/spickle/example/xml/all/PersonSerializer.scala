package com.github.rvanheest.spickle.example.xml.all

import com.github.rvanheest.spickle.example.xml.all.PersonSerializer.Person
import com.github.rvanheest.spickle.serializer.xml.XmlSerializer.{ XmlSerializer, _ }
import shapeless.HNil

import scala.util.Success
import scala.xml.PrettyPrinter

object PersonSerializerRunner extends App {

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

object PersonSerializer extends App {
  case class Person(firstName: String, lastName: String, age: Int)

  def serializeFirstname: XmlSerializer[String] = stringNode("firstname")

  def serializeLastname: XmlSerializer[String] = {
    stringNode("lastname").contramap[String](_.toLowerCase.capitalize)
  }

  def serializeAge: XmlSerializer[Int] = stringNode("age").fromInt

  def serializePersonContent: XmlSerializer[(String, String, Int)] = {
    fromAllMandatory(serializeAge)
      .andMandatory(serializeLastname)
      .andMandatory(serializeFirstname)
      .build
      .contramap[(String, String, Int)] { case (fn, ln, age) => fn :: ln :: age :: HNil }
  }

  def serializePerson: XmlSerializer[Person] = branchNode("person")(
    serializePersonContent.contramap[Person](p => (p.firstName, p.lastName, p.age))
  )

  def serializeSomeNumbers: XmlSerializer[Seq[Int]] = {
    stringNode("somenumbers").contramap[Seq[Int]](_.mkString(" "))
  }

  def serializePersons: XmlSerializer[(Seq[Person], Seq[Int])] = branchNode("persons")(
    serializePerson.many.contramap[(Seq[Person], Seq[Int])] { case (ps, _) => ps }
      .combine(serializeSomeNumbers.contramap[(Seq[Person], Seq[Int])] { case (_, ns) => ns })
  )
}
