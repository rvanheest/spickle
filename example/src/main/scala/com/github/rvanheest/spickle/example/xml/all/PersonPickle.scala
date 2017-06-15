package com.github.rvanheest.spickle.example.xml.all

import com.github.rvanheest.spickle.pickle.xml.XmlPickle.{ XmlPickle, _ }

object PersonPickle {
  case class Person(firstName: String, lastName: String, age: Int)

  def pickleFirstname: XmlPickle[String] = stringNode("firstname")

  def pickleLastname: XmlPickle[String] = stringNode("lastname").seq[String](_.toLowerCase.capitalize).map(_.toUpperCase)

  def pickleAge: XmlPickle[Int] = stringNode("age").seq[Int](_.toString).map(_.toInt)

  def picklePersonContent: XmlPickle[(String, String, Int)] = {
    all(pickleFirstname, pickleLastname, pickleAge)(mandatory, mandatory, mandatory)
  }

  def picklePerson: XmlPickle[Person] = {
    branchNode("person")(picklePersonContent.seq[Person](person => (person.firstName, person.lastName, person.age)).map(Person.tupled))
  }

  def picklePersons: XmlPickle[Seq[Person]] = branchNode("persons")(picklePerson.many)
}
