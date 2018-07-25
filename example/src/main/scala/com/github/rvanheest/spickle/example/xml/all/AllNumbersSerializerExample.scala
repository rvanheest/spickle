package com.github.rvanheest.spickle.example.xml.all

import com.github.rvanheest.spickle.example.xml.all.NumberSerializer.Numbers
import com.github.rvanheest.spickle.serializer.xml.AllSerializerBuilder
import com.github.rvanheest.spickle.serializer.xml.XmlSerializer.{ XmlSerializer, _ }
import shapeless.{ ::, HNil }

import scala.language.postfixOps
import scala.util.Success
import scala.xml.PrettyPrinter

object AllNumbersSerializerExample extends App {

  val numbers = Numbers(None, Option(2), None, Option(4), 5, 6, 7, 8, 9, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
    21, 22, 23, 24, 25, 26)

  val Success(xmls) = NumberSerializer.serializeNumbers.serialize(numbers, Seq.empty)
  for (xml <- xmls) {
    println(new PrettyPrinter(160, 2).format(xml))
  }
}

object NumberSerializer {

  case class Numbers(a: Option[Int], b: Option[Int], c: Option[Int], d: Option[Int], e: Int,
                     f: Int, g: Int, h: Int, i: Int, j: Int,
                     k: Int, l: Int, m: Int, n: Int, o: Int,
                     p: Int, q: Int, r: Int, s: Int, t: Int,
                     u: Int, v: Int, w: Int, x: Int, y: Int,
                     z: Int)

  def serializeNumbers: XmlSerializer[Numbers] = {
    branchNode("numbers")(serializeAllNumbers)
  }

  def serializeAllNumbers: XmlSerializer[Numbers] = {
    AllSerializerBuilder.fromOptional(serializeNumber('a'))
      .andOptional(serializeNumber('b'))
      .andOptional(serializeNumber('c'))
      .andOptional(serializeNumber('d'))
      .andMandatory(serializeNumber('e'))
      .andMandatory(serializeNumber('f'))
      .andMandatory(serializeNumber('g'))
      .andMandatory(serializeNumber('h'))
      .andMandatory(serializeNumber('i'))
      .andMandatory(serializeNumber('j'))
      .andMandatory(serializeNumber('k'))
      .andMandatory(serializeNumber('l'))
      .andMandatory(serializeNumber('m'))
      .andMandatory(serializeNumber('n'))
      .andMandatory(serializeNumber('o'))
      .andMandatory(serializeNumber('p'))
      .andMandatory(serializeNumber('q'))
      .andMandatory(serializeNumber('r'))
      .andMandatory(serializeNumber('s'))
      .andMandatory(serializeNumber('t'))
      .andMandatory(serializeNumber('u'))
      .andMandatory(serializeNumber('v'))
      .andMandatory(serializeNumber('w'))
      .andMandatory(serializeNumber('x'))
      .andMandatory(serializeNumber('y'))
      .andMandatory(serializeNumber('z'))
      .build
      .contramap[Option[Int] :: Option[Int] :: Option[Int] :: Option[Int] :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: HNil](_.reverse)
      .contramap[Numbers] {
        case Numbers(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22, p23, p24, p25, p26) =>
          p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 ::
          p11 :: p12 :: p13 :: p14 :: p15 :: p16 :: p17 :: p18 :: p19 :: p20 ::
          p21 :: p22 :: p23 :: p24 :: p25 :: p26 :: HNil
      }
  }

  def serializeNumber(label: Char): XmlSerializer[Int] = {
    stringNode(label.toString).fromInt
  }
}
