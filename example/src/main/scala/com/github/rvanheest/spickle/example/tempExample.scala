package com.github.rvanheest.spickle.example

import com.github.rvanheest.spickle.parser.xml.XmlParser

import scala.xml.Elem

object tempExample extends App {

  val xml: Elem = <hello>world</hello>
  println(XmlParser.xmlToString("hello").run(xml))
}
