package com.github.rvanheest.spickle.example.xml.person

import com.github.rvanheest.spickle.pickle.xml.XmlPickle._

import scala.xml.{ NamespaceBinding, TopScope }

trait PersonPickle {
  this: Person =>

  def pickleNumber(name: String): XmlPickle[Number] = {
    for {
      addition <- attribute("addition").maybe.seq[Number](_.addition)
      number <- stringNode(name).seq[Number](_.number)
    } yield Number(number, addition)
  }

  def pickleRealAddress(name: String): XmlPickle[RealAddress] = {
    branchNode(name) {
      for {
        street <- stringNode("street").seq[RealAddress](_.street)
        number <- pickleNumber("number").seq[RealAddress](_.number)
        zipCode <- stringNode("zip-code").seq[RealAddress](_.zipCode)
        city <- stringNode("city").seq[RealAddress](_.city)
      } yield RealAddress(street, number, zipCode, city)
    }
  }

  def pickleFreepostAddress(name: String): XmlPickle[FreepostAddress] = {
    branchNode(name) {
      for {
        number <- stringNode("freepost-number").seq[FreepostAddress](_.number)
        zipCode <- stringNode("zip-code").seq[FreepostAddress](_.zipCode)
        city <- stringNode("city").seq[FreepostAddress](_.city)
      } yield FreepostAddress(number, zipCode, city)
    }
  }

  def pickleAddress(name: String): XmlPickle[Address] = {
    pickleRealAddress(name).upcast[Address] orElse pickleFreepostAddress(name).upcast[Address]
  }

  def picklePerson: XmlPickle[Person] = {
    val xlinkNamespace = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    for {
      // ALWAYS put the attribute pickling first, BEFORE pickling the node's content
      age <- attribute("age").toInt.seq[Person](_.age)
      _ <- attribute("age", xlinkNamespace).toInt.seq[Person](_.age)
      p <- branchNode("person") {
        for {
          pName <- stringNode("name").seq[Person](_.name)
          address <- pickleAddress("address").seq[Person](_.address)
          mail <- stringNode("mail").maybe.seq[Person](_.mail)
        } yield Person(pName, age, address, mail)
      }.seqId
    } yield p
  }
}
