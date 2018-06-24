package com.github.rvanheest.spickle.example.xml.person

import com.github.rvanheest.spickle.serializer.xml.XmlSerializer._
import com.github.rvanheest.spickle.serializer.Serializer._

import scala.xml.{ NamespaceBinding, TopScope }

trait PersonSerializer {
  this: Person =>

  def serializerNumber(name: String): XmlSerializer[Number] = {
    attribute("addition").maybe.contramap[Number](_.addition)
      .combine(stringNode(name).contramap[Number](_.number))
  }

  def serializeRealAddress(name: String): XmlSerializer[RealAddress] = {
    branchNode(name)(
      stringNode("street").contramap[RealAddress](_.street)
        .combine(serializerNumber("number").contramap[RealAddress](_.number))
        .combine(stringNode("zip-code").contramap[RealAddress](_.zipCode))
        .combine(stringNode("city").contramap[RealAddress](_.city))
    )
  }

  def serializeFreepostAddress(name: String): XmlSerializer[FreepostAddress] = {
    branchNode(name)(
      stringNode("freepost-number").contramap[FreepostAddress](_.number)
        .combine(stringNode("zip-code").contramap[FreepostAddress](_.zipCode))
        .combine(stringNode("city").contramap[FreepostAddress](_.city))
    )
  }

  def serializeAddress(name: String): XmlSerializer[Address] = {
    serializeRealAddress(name).upcast[Address] orElse serializeFreepostAddress(name).upcast[Address]
  }

  def serializePerson: XmlSerializer[Person] = {
    implicit val xlinkNamespace = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)

    attribute("age").fromInt.contramap[Person](_.age)
      .combine(namespaceAttribute("age").fromInt.contramap[Person](_.age))
      .combine(branchNode("person") {
        stringNode("name").contramap[Person](_.name)
          .combine(serializeAddress("address").contramap[Person](_.address))
          .combine(stringNode("mail").maybe.contramap[Person](_.mail))
      })
  }
}
