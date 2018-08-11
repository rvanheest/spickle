package com.github.rvanheest.spickle.example.xml.xsChoiceMany

import com.github.rvanheest.spickle.example.xml.xsChoiceMany.XsChoiceMany._
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

  def serializeD1: XmlSerializer[D1Holder] = stringNode("d1").contramap(_.d1)

  def serializeD2: XmlSerializer[D2Holder] = {
    branchNode("d2") {
      stringNode("d2A").contramap[D2Holder](_.d2A)
        .combine(stringNode("d2B").maybe.contramap(_.d2B))
    }
  }

  def serializeE: XmlSerializer[EHolder] = {
    branchNode("e") {
      stringNode("a").contramap(_.a)
    }
  }

  def serializeF: XmlSerializer[FHolder] = {
    branchNode("f") {
      stringNode("a").contramap(_.a)
    }
  }
}
