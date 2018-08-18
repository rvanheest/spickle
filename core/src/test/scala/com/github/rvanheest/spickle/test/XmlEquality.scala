package com.github.rvanheest.spickle.test

import org.scalatest.Suite
import org.scalatest.matchers.{ MatchResult, Matcher }

import scala.util.{ Failure, Success, Try }
import scala.xml.{ Node, PrettyPrinter }

trait XmlEquality {
  this: Suite =>

  class EqualTrimmedMatcher(right: Iterable[Node]) extends Matcher[Try[Iterable[Node]]] {
    private val pp = new PrettyPrinter(160, 2)

    private def prepForTest(n: Node): String = pp.format(n)

    override def apply(left: Try[Iterable[Node]]): MatchResult = {
      left match {
        case Success(leftXml) =>
          val prettyLeft = leftXml.map(prepForTest)
          val prettyRight = right.map(prepForTest)

          lazy val prettyLeftString = prettyLeft.mkString("\n")
          lazy val prettyRightString = prettyRight.mkString("\n")

          MatchResult(
            prettyLeft == prettyRight,
            s"$prettyLeftString was not equal to $prettyRightString",
            s"$prettyLeftString was equal to $prettyRightString"
          )
        case Failure(e) =>
          fail(e)
      }
    }
  }
  def equalTrimmed(right: Iterable[Node]) = new EqualTrimmedMatcher(right)
}
