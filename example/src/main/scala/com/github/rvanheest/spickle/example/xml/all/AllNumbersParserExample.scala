package com.github.rvanheest.spickle.example.xml.all

import java.nio.file.Paths

import com.github.rvanheest.spickle.parser.xml.XmlParser.{ XmlParser, _ }
import shapeless.{ ::, HNil }

import scala.language.postfixOps
import scala.xml.{ Utility, XML }

object AllNumbersParserExample extends App {

  val path = Paths.get(getClass.getResource("/all/numbers.xml").toURI)
  val xml = Utility.trim(XML.loadFile(path.toFile))

  println(NumberParser.parseNumbers.parse(xml))
}

object NumberParser {

  case class Numbers(a: Option[Int], b: Option[Int], c: Option[Int], d: Option[Int], e: Int,
                     f: Int, g: Int, h: Int, i: Int, j: Int,
                     k: Int, l: Int, m: Int, n: Int, o: Int,
                     p: Int, q: Int, r: Int, s: Int, t: Int,
                     u: Int, v: Int, w: Int, x: Int, y: Int,
                     z: Int)

  def parseNumbers: XmlParser[Numbers] = {
    branchNode("numbers")(parseAllNumbers)
  }

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
      .build
      .map {
        case p26 :: p25 :: p24 :: p23 :: p22 :: p21 ::
          p20 :: p19 :: p18 :: p17 :: p16 :: p15 :: p14 :: p13 :: p12 :: p11 ::
          p10 :: p9 :: p8 :: p7 :: p6 :: p5 :: p4 :: p3 :: p2 :: p1 :: HNil =>
          Numbers(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
            p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
            p21, p22, p23, p24, p25, p26)
      }
  }

  def parseNumber(label: Char): XmlParser[Int] = {
    stringNode(label.toString).toInt
  }
}
