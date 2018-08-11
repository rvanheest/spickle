package com.github.rvanheest.spickle.example.xml.xsChoiceMany

import java.nio.file.Paths

import com.github.rvanheest.spickle.example.xml.xsChoiceMany.XsChoiceMany._
import com.github.rvanheest.spickle.parser.xml.XmlParser.{ XmlParser, _ }

import scala.language.postfixOps
import scala.util.Success
import scala.xml.{ Utility, XML }

object XsChoiceManyParserRunner extends App {

  val path1 = Paths.get(getClass.getResource("/xs-choice-many/xs-choice-many1.xml").toURI)
  val xml1 = Utility.trim(XML.loadFile(path1.toFile))
  val (Success(result1), remainder1) = XsChoiceManyParser.parseChoiceMany.parse(xml1)
  println(result1)
  println("remainder: " + (if (remainder1.isEmpty) "<none>" else remainder1))

  val path2 = Paths.get(getClass.getResource("/xs-choice-many/xs-choice-many2.xml").toURI)
  val xml2 = Utility.trim(XML.loadFile(path2.toFile))
  val (Success(result2), remainder2) = XsChoiceManyParser.parseChoiceMany.parse(xml2)
  println(result2)
  println("remainder: " + (if (remainder2.isEmpty) "<none>" else remainder2))
}

object XsChoiceManyParser {

  def parseChoiceMany: XmlParser[Data] = {
    branchNode("mixed") {
      for {
        e <- parseE
        a <- parseA
        // parsing choices starts here
        as <- collect(parseA)
        bs <- collect(parseB)
        cs <- collect(parseC)
        ds <- collect(parseD)
        // parsing choices ends here
        f <- parseF
      } yield Data(e, a, as, bs, cs, ds, f)
    }
  }

  def parseA: XmlParser[A] = stringNode("a")

  def parseB: XmlParser[B] = stringNode("b")

  def parseC: XmlParser[CHolder] = branchNode("c") {
    for {
      c1 <- stringNode("c1")
      c2 <- stringNode("c2")
    } yield CHolder(c1, c2)
  }

  def parseD: XmlParser[DHolder] = parseD1 <|> parseD2

  def parseD1: XmlParser[D1Holder] = stringNode("d1").map(D1Holder)

  def parseD2: XmlParser[D2Holder] = {
    branchNode("d2") {
      for {
        d2A <- stringNode("d2A")
        d2B <- stringNode("d2B").maybe
      } yield D2Holder(d2A, d2B)
    }
  }

  def parseE: XmlParser[EHolder] = {
    branchNode("e") {
      stringNode("a").map(EHolder)
    }
  }

  def parseF: XmlParser[FHolder] = {
    branchNode("f") {
      stringNode("a").map(EHolder)
    }
  }
}
