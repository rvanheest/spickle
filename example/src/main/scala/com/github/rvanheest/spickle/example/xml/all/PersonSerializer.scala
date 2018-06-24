package com.github.rvanheest.spickle.example.xml.all

import com.github.rvanheest.spickle.serializer.xml.XmlSerializer.{ XmlSerializer, _ }

object PersonSerializer {
  case class Person(firstName: String, lastName: String, age: Int)

  def serializeFirstname: XmlSerializer[String] = stringNode("firstname")

  def serializeLastname: XmlSerializer[String] = {
    stringNode("lastname").contramap[String](_.toLowerCase.capitalize)
  }

  def serializeAge: XmlSerializer[Int] = stringNode("age").fromInt

  def serializePersonContent: XmlSerializer[(String, String, Int)] = {
    all(serializeFirstname, serializeLastname, serializeAge)(mandatory, mandatory, mandatory)
  }

  def serializePerson: XmlSerializer[Person] = branchNode("person")(
    serializePersonContent.contramap[Person](p => (p.firstName, p.lastName, p.age))
  )

  def serializeSomeNumbers: XmlSerializer[Seq[Int]] = {
    stringNode("somenumbers").contramap[Seq[Int]](_.mkString(" "))
  }

  def serializePersons: XmlSerializer[(Seq[Person], Seq[Int])] = branchNode("persons")(
    serializePerson.many.contramap[(Seq[Person], Seq[Int])] { case (persons, _) => persons }
      .combine(serializeSomeNumbers.contramap[(Seq[Person], Seq[Int])] { case (_, nums) => nums })
  )
}
