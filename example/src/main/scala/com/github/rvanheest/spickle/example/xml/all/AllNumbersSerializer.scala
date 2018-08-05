package com.github.rvanheest.spickle.example.xml.all

import com.github.rvanheest.spickle.example.xml.all.AllNumberSerializer.Numbers
import com.github.rvanheest.spickle.serializer.xml.AllSerializerBuilder._
import com.github.rvanheest.spickle.serializer.xml.XmlSerializer.{ XmlSerializer, _ }
import shapeless.{ ::, Generic, HNil }

import scala.language.postfixOps
import scala.util.Success
import scala.xml.PrettyPrinter

object AllNumbersSerializerRunner extends App {

  val numbers = Numbers(None, Option(2), None, Option(4), 5, 6, 7, 8, 9, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
    21, 22, 23, 24, 25, 26)

  val Success(xmls) = AllNumberSerializer.serializeNumbers.serialize(numbers, Seq.empty)
  for (xml <- xmls) {
    println(new PrettyPrinter(160, 2).format(xml))
  }
}

object AllNumberSerializer {

  case class Numbers(a: Option[Int], b: Option[Int], c: Option[Int], d: Option[Int], e: Int,
                     f: Int, g: Int, h: Int, i: Int, j: Int,
                     k: Int, l: Int, m: Int, n: Int, o: Int,
                     p: Int, q: Int, r: Int, s: Int, t: Int,
                     u: Int, v: Int, w: Int, x: Int, y: Int,
                     z: Int)

  def serializeNumbers: XmlSerializer[Numbers] = {
    branchNode("numbers")(serializeAllNumbers)
  }

  // when the elements are defined in the same order as the class,
  // first _.reverse needs to be called
  def serializeAllNumbers: XmlSerializer[Numbers] = {
    fromOptional(serializeNumber('a'))
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
      .contramap[Option[Int] :: Option[Int] :: Option[Int] :: Option[Int] :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: HNil](_.reverse)
      .build(Generic[Numbers])
  }

  // when the elements are defined in reversed order, relative to the class,
  // _.reverse doesn't need to be called
  def serializeAllNumbers2: XmlSerializer[Numbers] = {
    fromAllMandatory(serializeNumber('z'))
      .andMandatory(serializeNumber('y'))
      .andMandatory(serializeNumber('x'))
      .andMandatory(serializeNumber('w'))
      .andMandatory(serializeNumber('v'))
      .andMandatory(serializeNumber('u'))
      .andMandatory(serializeNumber('t'))
      .andMandatory(serializeNumber('s'))
      .andMandatory(serializeNumber('r'))
      .andMandatory(serializeNumber('q'))
      .andMandatory(serializeNumber('p'))
      .andMandatory(serializeNumber('o'))
      .andMandatory(serializeNumber('n'))
      .andMandatory(serializeNumber('m'))
      .andMandatory(serializeNumber('l'))
      .andMandatory(serializeNumber('k'))
      .andMandatory(serializeNumber('j'))
      .andMandatory(serializeNumber('i'))
      .andMandatory(serializeNumber('h'))
      .andMandatory(serializeNumber('g'))
      .andMandatory(serializeNumber('f'))
      .andMandatory(serializeNumber('e'))
      .andOptional(serializeNumber('d'))
      .andOptional(serializeNumber('c'))
      .andOptional(serializeNumber('b'))
      .andOptional(serializeNumber('a'))
      .build(Generic[Numbers])
  }

  def serializeNumber(label: Char): XmlSerializer[Int] = {
    stringNode(label.toString).fromInt
  }
}
