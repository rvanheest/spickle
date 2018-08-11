package com.github.rvanheest.spickle.test.serializer.xml

import com.github.rvanheest.spickle.serializer.xml.XmlSerializer._
import org.scalatest.{ FlatSpec, Inside, Matchers }

import scala.util.Success

class XmlSerializerTest extends FlatSpec with Matchers with Inside {

  "collect" should "convert all input one-by-one and in order according to the given serializer" in {
    val input = Seq("test1", "test2", "test3", "test4")
    val serializer = collect(stringNode("abc"))

    inside(serializer.serialize(input)) {
      case Success(xml) =>
        xml should contain inOrderOnly (
          <abc>test1</abc>,
          <abc>test2</abc>,
          <abc>test3</abc>,
          <abc>test4</abc>,
        )
    }
  }

  it should "compose with other collect operators" in {
    val input = (Seq("test1", "test2", "test3", "test4"), Seq("random1", "random3"))

    val abcSerializer = collect(stringNode("abc"))
    val defSerializer = collect(stringNode("def"))
    val serializer = abcSerializer.contramap[(Seq[String], Seq[String])] { case (seq1, _) => seq1 }
      .combine(defSerializer.contramap { case (_, seq2) => seq2 } )

    inside(serializer.serialize(input)) {
      case Success(xml) =>
        xml should contain inOrderOnly (
          <abc>test1</abc>,
          <abc>test2</abc>,
          <abc>test3</abc>,
          <abc>test4</abc>,
          <def>random1</def>,
          <def>random3</def>,
        )
    }
  }

  it should "convert an empty list into an empty sequence of nodes" in {
    val input = Seq.empty[String]
    val serializer = collect(stringNode("abc"))

    serializer.serialize(input) should matchPattern { case Success(Seq()) => }
  }
}
