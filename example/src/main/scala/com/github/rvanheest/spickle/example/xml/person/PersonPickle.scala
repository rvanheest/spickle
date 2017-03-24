package com.github.rvanheest.spickle.example.xml.person

import com.github.rvanheest.spickle.pickle.xml.XmlPickle
import com.github.rvanheest.spickle.pickle.xml.XmlPickle._

import scala.xml.{ NamespaceBinding, TopScope }

trait PersonPickle {
  this: Person =>

  def pickleNumber(name: String): XmlPickle[Number] = {
    for {
      addition <- attribute("addition").maybe.seq[Number](_.addition)
      number <- string(name).seq[Number](_.number)
    } yield Number(number, addition)
  }

  def pickleRealAddress(name: String): XmlPickle[RealAddress] = {
    for {
      address <- branchNode(name) {
        for {
          street <- string("street").seq[RealAddress](_.street)
          number <- pickleNumber("number").seq[RealAddress](_.number)
          zipCode <- string("zip-code").seq[RealAddress](_.zipCode)
          city <- string("city").seq[RealAddress](_.city)
        } yield RealAddress(street, number, zipCode, city)
      }.seq
    } yield address
  }

  def pickleFreepostAddress(name: String): XmlPickle[FreepostAddress] = {
    for {
      address <- branchNode(name) {
        for {
          number <- string("freepost-number").seq[FreepostAddress](_.number)
          zipCode <- string("zip-code").seq[FreepostAddress](_.zipCode)
          city <- string("city").seq[FreepostAddress](_.city)
        } yield FreepostAddress(number, zipCode, city)
      }.seq
    } yield address
  }

  def pickleAddress(name: String): XmlPickle[Address] = {
    pickleRealAddress(name).upcast[Address] orElse pickleFreepostAddress(name).upcast[Address]
  }

  def picklePerson: XmlPickle[Person] = {
    implicit val xlinkNamespace = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    for {
      age <- attribute("age").toInt.seq[Person](_.age)
      _ <- namespaceAttribute("age").toInt.seq[Person](_.age)
      p <- branchNode("person") {
        for {
          pName <- string("name").seq[Person](_.name)
          address <- pickleAddress("address").seq[Person](_.address)
          mail <- string("mail").maybe.seq[Person](_.mail)
        } yield Person(pName, age, address, mail)
      }.seq
    } yield p
  }
}
