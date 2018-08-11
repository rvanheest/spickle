package com.github.rvanheest.spickle.example.xml.all

import java.nio.file.Paths

import com.github.rvanheest.spickle.example.xml.all.All.Numbers
import com.github.rvanheest.spickle.pickle.xml.XmlPickle.{ XmlPickle, _ }
import shapeless.{ ::, Generic, HNil }

import scala.language.postfixOps
import scala.util.Success
import scala.xml.{ Utility, XML }

object AllNumbersPickleRunner extends App {

  val path = Paths.get(getClass.getResource("/all/numbers.xml").toURI)
  val xml = Utility.trim(XML.loadFile(path.toFile))

  val parse @ (Success(numbers), rest) = AllNumberPickle.pickleNumbers.parse(xml)
  println(parse)

  val pickle @ Success(pickledXml) = AllNumberPickle.pickleNumbers.serialize(numbers, rest)
  println(pickle)

  val parse2 = AllNumberPickle.pickleNumbers.parse(pickledXml)
  println(parse2)
}

object AllNumberPickle {

  def pickleNumbers: XmlPickle[Numbers] = {
    branchNode("numbers")(pickleAllNumbers)
  }

  // when the elements are defined in the same order as the class,
  // first _.reverse needs to be called
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
      .seq[Option[Int] :: Option[Int] :: Option[Int] :: Option[Int] :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: Int :: HNil](_.reverse).map(_.reverse)
      .build(Generic[Numbers])
  }

  // when the elements are defined in reversed order, relative to the class,
  // _.reverse doesn't need to be called
  def pickleAllNumbers2: XmlPickle[Numbers] = {
    fromAllMandatory(pickleNumber('z'))
      .andMandatory(pickleNumber('y'))
      .andMandatory(pickleNumber('x'))
      .andMandatory(pickleNumber('w'))
      .andMandatory(pickleNumber('v'))
      .andMandatory(pickleNumber('u'))
      .andMandatory(pickleNumber('t'))
      .andMandatory(pickleNumber('s'))
      .andMandatory(pickleNumber('r'))
      .andMandatory(pickleNumber('q'))
      .andMandatory(pickleNumber('p'))
      .andMandatory(pickleNumber('o'))
      .andMandatory(pickleNumber('n'))
      .andMandatory(pickleNumber('m'))
      .andMandatory(pickleNumber('l'))
      .andMandatory(pickleNumber('k'))
      .andMandatory(pickleNumber('j'))
      .andMandatory(pickleNumber('i'))
      .andMandatory(pickleNumber('h'))
      .andMandatory(pickleNumber('g'))
      .andMandatory(pickleNumber('f'))
      .andMandatory(pickleNumber('e'))
      .andOptional(pickleNumber('d'))
      .andOptional(pickleNumber('c'))
      .andOptional(pickleNumber('b'))
      .andOptional(pickleNumber('a'))
      .build(Generic[Numbers])
  }

  def pickleNumber(label: Char): XmlPickle[Int] = {
    stringNode(label.toString).toInt
  }
}
