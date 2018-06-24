package com.github.rvanheest.spickle.example.xml.all

import com.github.rvanheest.spickle.pickle.xml.XmlPickle.{ XmlPickle, _ }
import com.github.rvanheest.spickle.pickle.Pickle._

object PersonPickle {
  case class Person(firstName: String, lastName: String, age: Int)

  def pickleFirstname: XmlPickle[String] = stringNode("firstname")

  def pickleLastname: XmlPickle[String] = stringNode("lastname").seq[String](_.toLowerCase.capitalize).map(_.toUpperCase)

  def pickleAge: XmlPickle[Int] = stringNode("age").toInt

  def picklePersonContent: XmlPickle[(String, String, Int)] = {
    all(pickleFirstname, pickleLastname, pickleAge)(mandatory, mandatory, mandatory)
  }

  def picklePerson: XmlPickle[Person] = {
    branchNode("person")(picklePersonContent.seq[Person](person => (person.firstName, person.lastName, person.age)).map(Person.tupled))
  }

  def pickleSomeNumbers: XmlPickle[Seq[Int]] = {
    stringNode("somenumbers")
      .seq[Seq[Int]](_.mkString(" "))
      .map(_.split(" ").map(_.toInt))
  }

  def picklePersons: XmlPickle[(Seq[Person], Seq[Int])] = branchNode("persons") {
    for {
      persons <- picklePerson.many.seq[(Seq[Person], Seq[Int])] { case (persons, _) => persons }
      numbers <- pickleSomeNumbers.seq[(Seq[Person], Seq[Int])] { case (_, nums) => nums }
    } yield (persons, numbers)
  }
}
