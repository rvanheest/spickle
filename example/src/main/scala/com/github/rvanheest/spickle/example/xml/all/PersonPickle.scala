package com.github.rvanheest.spickle.example.xml.all

import java.nio.file.Paths

import com.github.rvanheest.spickle.example.xml.all.All.Person
import com.github.rvanheest.spickle.pickle.Pickle._
import com.github.rvanheest.spickle.pickle.xml.XmlPickle.{ XmlPickle, _ }
import shapeless.HNil

import scala.util.Success
import scala.xml.{ Utility, XML }

object PersonPickleRunner extends App {

  val path = Paths.get(getClass.getResource("/all/person1.xml").toURI)
  val xml = Utility.trim(XML.loadFile(path.toFile))

  val parse @ (Success(persons), rest) = PersonPickle.picklePersons.parse(xml)
  println(parse)

  val pickle @ Success(pickledXml) = PersonPickle.picklePersons.serialize(persons, rest)
  println(pickle)

  val parse2 = PersonPickle.picklePersons.parse(pickledXml)
  println(parse2)
}

object PersonPickle {

  def pickleFirstname: XmlPickle[String] = stringNode("firstname")

  def pickleLastname: XmlPickle[String] = stringNode("lastname").seq[String](_.toLowerCase.capitalize).map(_.toUpperCase)

  def pickleAge: XmlPickle[Int] = stringNode("age").toInt

  def picklePersonContent: XmlPickle[(String, String, Int)] = {
    fromAllMandatory(pickleAge)
      .andMandatory(pickleLastname)
      .andMandatory(pickleFirstname)
      .build
      .seq[(String, String, Int)] { case (fn, ln, age) => fn :: ln :: age :: HNil }
      .map(_.tupled)
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
      persons <- picklePerson.many.seq[(Seq[Person], Seq[Int])] { case (ps, _) => ps }
      numbers <- pickleSomeNumbers.seq[(Seq[Person], Seq[Int])] { case (_, nums) => nums }
    } yield (persons, numbers)
  }
}
