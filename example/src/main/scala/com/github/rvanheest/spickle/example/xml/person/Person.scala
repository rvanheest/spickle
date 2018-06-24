package com.github.rvanheest.spickle.example.xml.person

trait Person {

  case class Number(number: String, addition: Option[String] = None)

  sealed abstract class Address(zipCode: String, city: String)
  case class RealAddress(street: String, number: Number, zipCode: String, city: String)
    extends Address(zipCode: String, city: String)
  case class FreepostAddress(number: String, zipCode: String, city: String)
    extends Address(zipCode: String, city: String)

  case class Person(name: String, age: Int, address: Address, mail: Option[String] = None)
}
