package com.github.rvanheest.spickle.example.xml.person

import scala.xml.{ Elem, Node, Utility }

trait PersonXml {
  this: Person =>

  val object1 = Person(
    name = "Jonathan Moreno",
    age = 24,
    address = RealAddress(
      street = "Atwood",
      number = Number("6689"),
      zipCode = "1234AB",
      city = "Tembladera"
    ),
    mail = Option("jmoreno5@webmd.com")
  )

  val object2 = Person(
    name = "Jonathan Moreno",
    age = 48,
    address = RealAddress(
      street = "Atwood",
      number = Number("6689", Option("a")),
      zipCode = "1234AB",
      city = "Tembladera"
    ),
    mail = Option("jmoreno5@webmd.com")
  )

  val object3 = Person(
    name = "Jonathan Moreno",
    age = 68,
    address = FreepostAddress(
      number = "70",
      zipCode = "1234AB",
      city = "Tembladera"
    )
  )

  // @formatter:off
  private val person1Xml: Elem = <person age="24" xlink:age="24">
    <name>Jonathan Moreno</name>
    <address>
      <street>Atwood</street>
      <number>6689</number>
      <zip-code>1234AB</zip-code>
      <city>Tembladera</city>
    </address>
    <mail>jmoreno5@webmd.com</mail>
  </person>

  private val person2Xml: Elem = <person age="48" xlink:age="48">
    <name>Jonathan Moreno</name>
    <address>
      <street>Atwood</street>
      <number addition="a">6689</number>
      <zip-code>1234AB</zip-code>
      <city>Tembladera</city>
    </address>
    <mail>jmoreno5@webmd.com</mail>
  </person>

  private val person3Xml: Elem = <person age="68" xlink:age="68">
    <name>Jonathan Moreno</name>
    <address>
      <freepost-number>70</freepost-number>
      <zip-code>1234AB</zip-code>
      <city>Tembladera</city>
    </address>
  </person>
  // @formatter:on

  val xml1: Node = Utility.trim(person1Xml)
  val xml2: Node = Utility.trim(person2Xml)
  val xml3: Node = Utility.trim(person3Xml)
}
