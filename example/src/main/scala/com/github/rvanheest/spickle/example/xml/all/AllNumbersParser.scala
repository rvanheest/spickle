package com.github.rvanheest.spickle.example.xml.all

import java.nio.file.Paths

import com.github.rvanheest.spickle.parser.xml.XmlParser.{ XmlParser, _ }
import shapeless.Generic

import scala.language.postfixOps
import scala.xml.{ Utility, XML }

object AllNumbersParserRunner extends App {

  val path1 = Paths.get(getClass.getResource("/all/numbers1.xml").toURI)
  val xml1 = Utility.trim(XML.loadFile(path1.toFile))

  println(AllNumberParser.parseNumbers.parse(xml1))

  val path2 = Paths.get(getClass.getResource("/all/numbers2.xml").toURI)
  val xml2 = Utility.trim(XML.loadFile(path2.toFile))

  println(AllNumberParser.parseNumbers.parse(xml2))
}

object AllNumberParser {

  case class Numbers(a: Option[Int], b: Option[Int], c: Option[Int], d: Option[Int], e: Int,
                     f: Int, g: Int, h: Int, i: Int, j: Int,
                     k: Int, l: Int, m: Int, n: Int, o: Int,
                     p: Int, q: Int, r: Int, s: Int, t: Int,
                     u: Int, v: Int, w: Int, x: Int, y: Int,
                     z: Int)

  def parseNumbers: XmlParser[Numbers] = {
    branchNode("numbers")(parseAllNumbers)
  }

  // when the elements are defined in the same order as the class,
  // first _.reverse needs to be called
  def parseAllNumbers: XmlParser[Numbers] = {
    fromAllOptional(parseNumber('a'))
      .andOptional(parseNumber('b'))
      .andOptional(parseNumber('c'))
      .andOptional(parseNumber('d'))
      .andMandatory(parseNumber('e'))
      .andMandatory(parseNumber('f'))
      .andMandatory(parseNumber('g'))
      .andMandatory(parseNumber('h'))
      .andMandatory(parseNumber('i'))
      .andMandatory(parseNumber('j'))
      .andMandatory(parseNumber('k'))
      .andMandatory(parseNumber('l'))
      .andMandatory(parseNumber('m'))
      .andMandatory(parseNumber('n'))
      .andMandatory(parseNumber('o'))
      .andMandatory(parseNumber('p'))
      .andMandatory(parseNumber('q'))
      .andMandatory(parseNumber('r'))
      .andMandatory(parseNumber('s'))
      .andMandatory(parseNumber('t'))
      .andMandatory(parseNumber('u'))
      .andMandatory(parseNumber('v'))
      .andMandatory(parseNumber('w'))
      .andMandatory(parseNumber('x'))
      .andMandatory(parseNumber('y'))
      .andMandatory(parseNumber('z'))
      .map(_.reverse)
      .build(Generic[Numbers])
  }

  // when the elements are defined in reversed order, relative to the class,
  // _.reverse doesn't need to be called
  def parseAllNumbers2: XmlParser[Numbers] = {
    fromAllMandatory(parseNumber('z'))
      .andMandatory(parseNumber('y'))
      .andMandatory(parseNumber('x'))
      .andMandatory(parseNumber('w'))
      .andMandatory(parseNumber('v'))
      .andMandatory(parseNumber('u'))
      .andMandatory(parseNumber('t'))
      .andMandatory(parseNumber('s'))
      .andMandatory(parseNumber('r'))
      .andMandatory(parseNumber('q'))
      .andMandatory(parseNumber('p'))
      .andMandatory(parseNumber('o'))
      .andMandatory(parseNumber('n'))
      .andMandatory(parseNumber('m'))
      .andMandatory(parseNumber('l'))
      .andMandatory(parseNumber('k'))
      .andMandatory(parseNumber('j'))
      .andMandatory(parseNumber('i'))
      .andMandatory(parseNumber('h'))
      .andMandatory(parseNumber('g'))
      .andMandatory(parseNumber('f'))
      .andMandatory(parseNumber('e'))
      .andOptional(parseNumber('d'))
      .andOptional(parseNumber('c'))
      .andOptional(parseNumber('b'))
      .andOptional(parseNumber('a'))
      .build(Generic[Numbers])
  }

  def parseNumber(label: Char): XmlParser[Int] = {
    stringNode(label.toString).toInt
  }
}
