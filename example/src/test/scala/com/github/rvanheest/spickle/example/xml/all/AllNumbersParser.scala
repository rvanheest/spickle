package com.github.rvanheest.spickle.example.xml.all

import java.nio.file.Paths

import com.github.rvanheest.spickle.example.xml.all.AllNumberParser._
import com.github.rvanheest.spickle.parser.ParserFailedException
import org.scalatest.{ FlatSpec, Inside, Matchers, OptionValues }

import scala.util.{ Failure, Success }
import scala.xml.transform.{ RewriteRule, RuleTransformer }
import scala.xml._

class AllNumbersParser extends FlatSpec with Matchers with OptionValues with Inside {

  "parseNumbers" should "parse when the optional values are all present" in {
    val path = Paths.get(getClass.getResource("/all/numbers1.xml").toURI)
    val xml = Utility.trim(XML.loadFile(path.toFile))

    inside(parseNumbers.parse(xml)) {
      case (Success(result), remainder) =>
        remainder shouldBe empty

        result.a.value shouldBe 1
        result.b.value shouldBe 2
        result.c.value shouldBe 3
        result.d.value shouldBe 4
        result.e shouldBe 5
        result.f shouldBe 6
        result.g shouldBe 7
        result.h shouldBe 8
        result.i shouldBe 9
        result.j shouldBe 10
        result.k shouldBe 11
        result.l shouldBe 12
        result.m shouldBe 13
        result.n shouldBe 14
        result.o shouldBe 15
        result.p shouldBe 16
        result.q shouldBe 17
        result.r shouldBe 18
        result.s shouldBe 19
        result.t shouldBe 20
        result.u shouldBe 21
        result.v shouldBe 22
        result.w shouldBe 23
        result.x shouldBe 24
        result.y shouldBe 25
        result.z shouldBe 26
    }
  }

  it should "parse when the optional values are not all present" in {
    val path = Paths.get(getClass.getResource("/all/numbers2.xml").toURI)
    val xml = Utility.trim(XML.loadFile(path.toFile))

    inside(parseNumbers.parse(xml)) {
      case (Success(result), remainder) =>
        remainder shouldBe empty

        result.a shouldBe empty
        result.b.value shouldBe 2
        result.c shouldBe empty
        result.d.value shouldBe 4
        result.e shouldBe 5
        result.f shouldBe 6
        result.g shouldBe 7
        result.h shouldBe 8
        result.i shouldBe 9
        result.j shouldBe 10
        result.k shouldBe 11
        result.l shouldBe 12
        result.m shouldBe 13
        result.n shouldBe 14
        result.o shouldBe 15
        result.p shouldBe 16
        result.q shouldBe 17
        result.r shouldBe 18
        result.s shouldBe 19
        result.t shouldBe 20
        result.u shouldBe 21
        result.v shouldBe 22
        result.w shouldBe 23
        result.x shouldBe 24
        result.y shouldBe 25
        result.z shouldBe 26
    }
  }

  "parseAllNumbers" should "keep 'other' as a remainder" in {
    val path = Paths.get(getClass.getResource("/all/numbers1.xml").toURI)
    val xml = Utility.trim(XML.loadFile(path.toFile))

    inside(parseAllNumbers.parse(xml.child)) {
      case (Success(_), remainder) =>
        remainder should contain only <other>27</other>
    }
  }

  it should "fail when a mandatory field is not present" in {
    val path = Paths.get(getClass.getResource("/all/numbers1.xml").toURI)
    val xml = Utility.trim(XML.loadFile(path.toFile))

    val xmlWithoutX = new RuleTransformer(new RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case Elem(_, "x", _, _, _) =>
          NodeSeq.Empty
        case _ => n
      }
    }).transform(xml).head

    inside(parseAllNumbers.parse(xmlWithoutX.child)) {
      case (Failure(e: ParserFailedException), remainder) =>
        val expectedRemainder = xml.child
            .filterNot(_.label == "z")
            .filterNot(_.label == "y")
            .filterNot(_.label == "x")
        remainder should contain theSameElementsAs expectedRemainder

        e should have message "missing mandatory element in any"
    }
  }
}
