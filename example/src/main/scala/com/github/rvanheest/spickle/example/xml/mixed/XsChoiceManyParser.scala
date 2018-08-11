package com.github.rvanheest.spickle.example.xml.mixed

import java.nio.file.Paths

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

  type A = String
  type B = String
  type C1 = String
  type C2 = String
  type D1 = String
  type D2A = String
  type D2B = Option[String]

  case class CHolder(c1: C1, c2: C1)

  abstract class DHolder
  case class D1Holder(d1: D1) extends DHolder
  case class D2Holder(d2A: D2A, d2B: D2B) extends DHolder

  case class EHolder(a: A)
  type FHolder = EHolder

  case class Data(e: EHolder, a: A, as: Seq[A], bs: Seq[B], cs: Seq[CHolder], ds: Seq[DHolder], f: FHolder) {
    override def toString: String = {
      s"""Mixed
         |  e:  $e
         |  a:  $a
         |  as: ${ if (as.isEmpty) "<none>" else as.mkString("\n    - ", "\n    - ", "") }
         |  bs: ${ if (bs.isEmpty) "<none>" else bs.mkString("\n    - ", "\n    - ", "") }
         |  cs: ${ if (cs.isEmpty) "<none>" else cs.mkString("\n    - ", "\n    - ", "") }
         |  ds: ${ if (ds.isEmpty) "<none>" else ds.mkString("\n    - ", "\n    - ", "") }
         |  f:  $f""".stripMargin
    }
  }

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
