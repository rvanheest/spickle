package com.github.rvanheest.spickle.example.xml.person

import scala.xml.{ Elem, Node, Utility }

trait PersonXml {

  // @formatter:off
  private val person1: Elem = <person age="24" xlink:age="24">
    <name>Jonathan Moreno</name>
    <address>
      <street>Atwood</street>
      <number>6689</number>
      <zip-code>1234AB</zip-code>
      <city>Tembladera</city>
    </address>
    <mail>jmoreno5@webmd.com</mail>
  </person>

  private val person2: Elem = <person age="48" xlink:age="48">
    <name>Jonathan Moreno</name>
    <address>
      <street>Atwood</street>
      <number addition="a">6689</number>
      <zip-code>1234AB</zip-code>
      <city>Tembladera</city>
    </address>
    <mail>jmoreno5@webmd.com</mail>
  </person>

  private val person3: Elem = <person age="68" xlink:age="68">
    <name>Jonathan Moreno</name>
    <address>
      <freepost-number>70</freepost-number>
      <zip-code>1234AB</zip-code>
      <city>Tembladera</city>
    </address>
  </person>
  // @formatter:on

  val xml1: Node = Utility.trim(person1)
  val xml2: Node = Utility.trim(person2)
  val xml3: Node = Utility.trim(person3)
}
