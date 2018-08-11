package com.github.rvanheest.spickle.example.xml.xsChoiceMany

import com.github.rvanheest.spickle.example.xml.xsChoiceMany.XsChoiceManySerializer._
import com.github.rvanheest.spickle.serializer.xml.XmlSerializer.{ XmlSerializer, _ }

import scala.language.postfixOps
import scala.util.Success
import scala.xml.PrettyPrinter

object XsChoiceManySerializerRunner extends App {

  val input1 = Data(
    EHolder("abcdef"),
    "xyz",
    Seq("a-A", "a-B"),
    Seq("b-A", "b-B"),
    Seq(
      CHolder("c-c1-A", "c-c2-A"),
      CHolder("c-c1-B", "c-c2-B"),
      CHolder("c-c1-C", "c-c2-C")
    ),
    Seq(
      D1Holder("d1-A"),
      D1Holder("d1-B"),
      D2Holder("d2-d2A-A", Option("d2-d2B-A")),
      D2Holder("d2-d2A-B", None)
    ),
    EHolder("uvwxyz")
  )
  val Success(Seq(result1)) = XsChoiceManySerializer.serializeChoiceMany.serialize(input1)
  println(new PrettyPrinter(160, 2).format(result1))

  val input2 = Data(
    EHolder("abcdef"),
    "klm",
    Seq.empty,
    Seq.empty,
    Seq.empty,
    Seq.empty,
    EHolder("xyz")
  )
  val Success(Seq(result2)) = XsChoiceManySerializer.serializeChoiceMany.serialize(input2)
  println(new PrettyPrinter(160, 2).format(result2))
}

object XsChoiceManySerializer {

  type A = String
  type B = String
  type C1 = String
  type C2 = String
  type D1 = String
  type D2A = String
  type D2B = Option[String]

  case class CHolder(c1: C1, c2: C2)

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

  def serializeChoiceMany: XmlSerializer[Data] = {
    branchNode("mixed") {
      serializeE.contramap[Data](_.e)
        .combine(serializeA.contramap(_.a))
        // parsing choices starts here
        .combine(collect(serializeA).contramap(_.as))
        .combine(collect(serializeB).contramap(_.bs))
        .combine(collect(serializeC).contramap(_.cs))
        .combine(collect(serializeD).contramap(_.ds))
        // parsing choices ends here
        .combine(serializeF.contramap(_.f))
    }
  }

  def serializeA: XmlSerializer[A] = stringNode("a")

  def serializeB: XmlSerializer[B] = stringNode("b")

  def serializeC: XmlSerializer[CHolder] = branchNode("c") {
    stringNode("c1").contramap[CHolder](_.c1)
      .combine(stringNode("c2").contramap(_.c2))
  }

  def serializeD: XmlSerializer[DHolder] = serializeD1.upcast[DHolder] orElse serializeD2.upcast[DHolder]

  def serializeD1: XmlSerializer[D1Holder] = stringNode("d1").contramap[D1Holder](_.d1)

  def serializeD2: XmlSerializer[D2Holder] = {
    branchNode("d2") {
      stringNode("d2A").contramap[D2Holder](_.d2A)
        .combine(stringNode("d2B").maybe.contramap(_.d2B))
    }
  }

  def serializeE: XmlSerializer[EHolder] = {
    branchNode("e") {
      stringNode("a").contramap[EHolder](_.a)
    }
  }

  def serializeF: XmlSerializer[FHolder] = {
    branchNode("f") {
      stringNode("a").contramap[FHolder](_.a)
    }
  }
}
