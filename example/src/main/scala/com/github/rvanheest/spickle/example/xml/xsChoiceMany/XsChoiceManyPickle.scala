package com.github.rvanheest.spickle.example.xml.xsChoiceMany

import java.nio.file.Paths

import com.github.rvanheest.spickle.example.xml.xsChoiceMany.XsChoiceMany._
import com.github.rvanheest.spickle.pickle.xml.XmlPickle.{ XmlPickle, _ }

import scala.language.postfixOps
import scala.util.Success
import scala.xml.{ PrettyPrinter, Utility, XML }

object XsChoiceManyPickleRunner extends App {

  val path1 = Paths.get(getClass.getResource("/xs-choice-many/xs-choice-many1.xml").toURI)
  val xml1 = Utility.trim(XML.loadFile(path1.toFile))
  val (Success(result1), remainder1) = XsChoiceManyPickle.pickleChoiceMany.parse(xml1)
  println(result1)
  println("remainder: " + (if (remainder1.isEmpty) "<none>" else remainder1))

  val Success(Seq(xmlResult1)) = XsChoiceManyPickle.pickleChoiceMany.serialize(result1)
  println(new PrettyPrinter(160, 2).format(xmlResult1))

  val (Success(result1a), remainder1a) = XsChoiceManyPickle.pickleChoiceMany.parse(xmlResult1)
  println(s"equal with previous result: ${ result1 == result1a }")
  println("remainder: " + (if (remainder1a.isEmpty) "<none>" else remainder1a))

  val path2 = Paths.get(getClass.getResource("/xs-choice-many/xs-choice-many2.xml").toURI)
  val xml2 = Utility.trim(XML.loadFile(path2.toFile))
  val (Success(result2), remainder2) = XsChoiceManyPickle.pickleChoiceMany.parse(xml2)
  println(result2)
  println("remainder: " + (if (remainder2.isEmpty) "<none>" else remainder2))

  val Success(Seq(xmlResult2)) = XsChoiceManyPickle.pickleChoiceMany.serialize(result2)
  println(new PrettyPrinter(160, 2).format(xmlResult2))

  val (Success(result2a), remainder2a) = XsChoiceManyPickle.pickleChoiceMany.parse(xmlResult2)
  println(s"equal with previous result: ${ result2 == result2a }")
  println("remainder: " + (if (remainder2a.isEmpty) "<none>" else remainder2a))
}

object XsChoiceManyPickle {

  def pickleChoiceMany: XmlPickle[Data] = {
    branchNode("mixed") {
      for {
        e <- parseE.seq[Data](_.e)
        a <- parseA.seq[Data](_.a)
        // parsing choices starts here
        as <- collect(parseA).seq[Data](_.as)
        bs <- collect(parseB).seq[Data](_.bs)
        cs <- collect(parseC).seq[Data](_.cs)
        ds <- collect(parseD).seq[Data](_.ds)
        // parsing choices ends here
        f <- parseF.seq[Data](_.f)
      } yield Data(e, a, as, bs, cs, ds, f)
    }
  }

  def parseA: XmlPickle[A] = stringNode("a")

  def parseB: XmlPickle[B] = stringNode("b")

  def parseC: XmlPickle[CHolder] = branchNode("c") {
    for {
      c1 <- stringNode("c1").seq[CHolder](_.c1)
      c2 <- stringNode("c2").seq[CHolder](_.c2)
    } yield CHolder(c1, c2)
  }

  def parseD: XmlPickle[DHolder] = parseD1.upcast[DHolder] orElse parseD2.upcast[DHolder]

  def parseD1: XmlPickle[D1Holder] = stringNode("d1").seq[D1Holder](_.d1).map(D1Holder)

  def parseD2: XmlPickle[D2Holder] = {
    branchNode("d2") {
      for {
        d2A <- stringNode("d2A").seq[D2Holder](_.d2A)
        d2B <- stringNode("d2B").maybe.seq[D2Holder](_.d2B)
      } yield D2Holder(d2A, d2B)
    }
  }

  def parseE: XmlPickle[EHolder] = {
    branchNode("e") {
      stringNode("a").seq[EHolder](_.a).map(EHolder)
    }
  }

  def parseF: XmlPickle[FHolder] = {
    branchNode("f") {
      stringNode("a").seq[EHolder](_.a).map(EHolder)
    }
  }
}
