package com.github.rvanheest.spickle.example.xml.person

import scala.xml.Elem

trait PersonXml {

  // @formatter:off
  val xml1: Elem = <person age="24" xlink:age="24">
    <name>Richard van Heest</name>
    <address>
      <street>Prins Bernhardlaan</street>
      <number>116</number>
      <zip-code>3241TA</zip-code>
      <city>Middelharnis</city>
    </address>
    <mail>richard.v.heest@gmail.com</mail>
  </person>

  val xml2: Elem = <person age="24" xlink:age="24">
    <name>Richard van Heest</name>
    <address>
      <street>Prins Bernhardlaan</street>
      <number addition="a">116</number>
      <zip-code>3241TA</zip-code>
      <city>Middelharnis</city>
    </address>
    <mail>richard.v.heest@gmail.com</mail>
  </person>

  val xml3: Elem = <person age="24" xlink:age="24">
    <name>Richard van Heest</name>
    <address>
      <freepost-number>12345</freepost-number>
      <zip-code>3241TA</zip-code>
      <city>Middelharnis</city>
    </address>
  </person>
  // @formatter:on
}
