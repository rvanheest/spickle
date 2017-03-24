package com.github.rvanheest.spickle.example.xml.person

import scala.xml.{ Elem, Node, Utility }

trait PersonXml {

  // @formatter:off
  private val person1: Elem = <person age="24" xlink:age="24">
    <name>Richard van Heest</name>
    <address>
      <street>Prins Bernhardlaan</street>
      <number>116</number>
      <zip-code>3241TA</zip-code>
      <city>Middelharnis</city>
    </address>
    <mail>richard.v.heest@gmail.com</mail>
  </person>

  private val person2: Elem = <person age="24" xlink:age="24">
    <name>Richard van Heest</name>
    <address>
      <street>Prins Bernhardlaan</street>
      <number addition="a">116</number>
      <zip-code>3241TA</zip-code>
      <city>Middelharnis</city>
    </address>
    <mail>richard.v.heest@gmail.com</mail>
  </person>

  private val person3: Elem = <person age="24" xlink:age="24">
    <name>Richard van Heest</name>
    <address>
      <freepost-number>12345</freepost-number>
      <zip-code>3241TA</zip-code>
      <city>Middelharnis</city>
    </address>
  </person>
  // @formatter:on

  val xml1: Node = Utility.trim(person1)
  val xml2: Node = Utility.trim(person2)
  val xml3: Node = Utility.trim(person3)
}
