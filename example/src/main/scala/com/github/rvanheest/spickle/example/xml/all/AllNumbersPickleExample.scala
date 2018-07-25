package com.github.rvanheest.spickle.example.xml.all

import java.nio.file.Paths

import com.github.rvanheest.spickle.pickle.xml.XmlPickle.{ XmlPickle, _ }
import shapeless.{ ::, HNil }

import scala.language.postfixOps
import scala.util.Success
import scala.xml.{ Utility, XML }

object AllNumbersPickleExample extends App {

  val path = Paths.get(getClass.getResource("/all/numbers.xml").toURI)
  val xml = Utility.trim(XML.loadFile(path.toFile))

  val parse @ (Success(numbers), rest) = NumberPickle.parseNumbers.parse(xml)
  println(parse)

  val pickle @ Success(pickledXml) = NumberPickle.parseNumbers.serialize(numbers, rest)
  println(pickle)

  val parse2 = NumberPickle.parseNumbers.parse(pickledXml)
  println(parse2)
}

object NumberPickle {

  case class Numbers(a: Option[Int], b: Option[Int], c: Option[Int], d: Option[Int], e: Int,
                     f: Int, g: Int, h: Int, i: Int, j: Int,
                     k: Int, l: Int, m: Int, n: Int, o: Int,
                     p: Int, q: Int, r: Int, s: Int, t: Int,
                     u: Int, v: Int, w: Int, x: Int, y: Int,
                     z: Int)

  def parseNumbers: XmlPickle[Numbers] = {
    branchNode("numbers")(pickleAllNumbers)
  }

  def pickleAllNumbers: XmlPickle[Numbers] = {
    fromAllOptional(pickleNumber('a'))
      .andOptional(pickleNumber('b'))
      .andOptional(pickleNumber('c'))
      .andOptional(pickleNumber('d'))
      .andMandatory(pickleNumber('e'))
      .andMandatory(pickleNumber('f'))
      .andMandatory(pickleNumber('g'))
      .andMandatory(pickleNumber('h'))
      .andMandatory(pickleNumber('i'))
      .andMandatory(pickleNumber('j'))
      .andMandatory(pickleNumber('k'))
      .andMandatory(pickleNumber('l'))
      .andMandatory(pickleNumber('m'))
      .andMandatory(pickleNumber('n'))
      .andMandatory(pickleNumber('o'))
      .andMandatory(pickleNumber('p'))
      .andMandatory(pickleNumber('q'))
      .andMandatory(pickleNumber('r'))
      .andMandatory(pickleNumber('s'))
      .andMandatory(pickleNumber('t'))
      .andMandatory(pickleNumber('u'))
      .andMandatory(pickleNumber('v'))
      .andMandatory(pickleNumber('w'))
      .andMandatory(pickleNumber('x'))
      .andMandatory(pickleNumber('y'))
      .andMandatory(pickleNumber('z'))
      .build
      .seq[Numbers] {
        case Numbers(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26) =>
          p26 :: p25 :: p24 :: p23 :: p22 :: p21 :: p20 :: p19 :: p18 :: p17 :: p16 :: p15 :: p14 :: p13 :: p12 :: p11 :: p10 :: p9 :: p8 :: p7 :: p6 :: p5 :: p4 :: p3 :: p2 :: p1 :: HNil
      }
      .map {
        case p26 :: p25 :: p24 :: p23 :: p22 :: p21 ::
          p20 :: p19 :: p18 :: p17 :: p16 :: p15 :: p14 :: p13 :: p12 :: p11 ::
          p10 :: p9 :: p8 :: p7 :: p6 :: p5 :: p4 :: p3 :: p2 :: p1 :: HNil =>
          Numbers(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
            p11, p12, p13, p14, p15, p16, p17, p18, p19, p20,
            p21, p22, p23, p24, p25, p26)
      }
  }

  def pickleNumber(label: Char): XmlPickle[Int] = {
    stringNode(label.toString).toInt
  }
}
