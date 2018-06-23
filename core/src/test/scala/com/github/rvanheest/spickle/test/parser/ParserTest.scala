package com.github.rvanheest.spickle.test.parser

import com.github.rvanheest.spickle.parser.{ Parser, ParserFailedException }
import org.scalatest.{ FlatSpec, Matchers }

import scala.language.postfixOps
import scala.util.{ Failure, Success, Try }

class ParserTest extends FlatSpec with Matchers {

  private val emptyError = ParserFailedException("you're trying to parse a character in an empty String")

  private type TestParser = Parser[String, Int]

  private def point: TestParser = new TestParser(_.toList match {
    case Nil => (Failure(emptyError), "")
    case p :: ps => (Try(p.toString.toInt), ps.mkString)
  })

  "run" should "apply the function given to the parser and return both the result and the remaining state" in {
    point.parse("123") should matchPattern { case (Success(1), "23") => }
  }

  "eval" should "apply the function given to the parser and return only the result" in {
    point.eval("123") should matchPattern { case Success(1) => }
  }

  "execute" should "apply the function given to the parser and return only the remaining state" in {
    point.execute("123") shouldBe "23"
  }

  "orElse" should "only run the first parser if it returns a success" in {
    point.orElse(Parser[String, Int](s => { fail("this position should not be visited"); (Failure(new Exception), s) }))
      .parse("123") should matchPattern { case (Success(1), "23") => }
  }

  it should "run the second parser if the first parser returns a failure" in {
    Parser.failure[String, Int](new Exception("ex"))
      .orElse(point)
      .parse("123") should matchPattern { case (Success(1), "23") => }
  }

  it should "run the second parser if the first parser returns a failure and return the failure of the second parser if that one fails as well" in {
    val ex = new Exception("ex")
    point.orElse(Parser.failure[String, Int](ex))
      .parse("a123") should matchPattern { case (Failure(`ex`), "a123") => }
  }

  it should "run the second parser with the same input as the first when the first parser fails" in {
    point.flatMap(_ => Parser.failure[String, Int](new Exception("ex")))
      .orElse(point)
      .parse("123") should matchPattern { case (Success(1), "23") => }
  }

  "map" should "apply the function in map when the parser returns a success" in {
    point.map(2 *).parse("123") should matchPattern { case (Success(2), "23") => }
  }

  it should "not apply the function in map when the parser returns a failure" in {
    point.map(i => { fail("this position should not be visited"); i * 2 })
      .parse("a123") should matchPattern { case (Failure(_: NumberFormatException), "123") => }
  }

  it should "fail if the function in map throws an exception" in {
    point.map(i => i / 0).parse("123") should matchPattern { case (Failure(_: ArithmeticException), "23") => }
  }

  "flatMap" should "apply the function in flatMap when the parser returns a success" in {
    point.flatMap(i => point.map(10 * i +))
      .parse("123") should matchPattern { case (Success(12), "3") => }
  }

  it should "apply the function in flatMap when the first parser returns a success even if the inner parser produces a failure" in {
    val error = new Exception("error")

    point.flatMap(_ => Parser.failure[String, Int](error))
      .parse("123") should matchPattern { case (Failure(`error`), "23") => }
  }

  it should "not apply the function in flatMap when the first parser returns a failure" in {
    point.flatMap(i => { fail("this position should not be visited"); point.map(i +) })
      .parse("a123") should matchPattern { case (Failure(_: NumberFormatException), "123") => }
  }

  "transform" should "apply the function in transform when the parser returns a success" in {
    point.transform((i, s) => (if (i == 1) Success(i + 1)
                               else Failure(new Exception("error!")), i + s))
      .parse("123") should matchPattern { case (Success(2), "123") => }
  }

  it should "not apply the function in transform when the parser returns a failure" in {
    val error = new Exception("error!")

    point.transform((i, s) => (if (i == 1) Success(i + 1)
                               else Failure(error), i + s))
      .parse("234") should matchPattern { case (Failure(`error`), "234") => }
  }

  "satisfy" should "produce a succeeding parser if the predicate succeeds" in {
    point.satisfy(_ % 2 == 1).parse("123") should matchPattern { case (Success(1), "23") => }
  }

  it should "produce a failing parser if the predicate fails" in {
    val expectedMsg = "input '1' did not satisfy predicate"

    point.satisfy(_ % 2 == 0)
      .parse("123") should matchPattern { case (Failure(ParserFailedException(`expectedMsg`)), "23") => }
  }

  it should "not apply the predicate if the parser fails" in {
    point.satisfy(i => { fail("this position should not be visited"); i % 2 == 0 })
      .parse("") should matchPattern { case (Failure(`emptyError`), "") => }
  }

  "noneOf" should "create a parser that succeeds when the value is not in the given list" in {
    point.noneOf(List(2, 3, 4)).parse("123") should matchPattern { case (Success(1), "23") => }
  }

  it should "create a parser that fails when the value is in the given list" in {
    val expectedMsg = "input '1' is contained in [0, 1, 2]"

    point.noneOf(List(0, 1, 2))
      .parse("123") should matchPattern { case (Failure(ParserFailedException(`expectedMsg`)), "23") => }
  }

  it should "fail if the parser was not able to parse the input" in {
    point.noneOf(List(0, 1, 2))
      .parse("a123") should matchPattern { case (Failure(_: NumberFormatException), "123") => }
  }

  "maybe" should "create a parser that succeeds with the value wrapped in an Option" in {
    point.maybe.parse("123") should matchPattern { case (Success(Some(1)), "23") => }
  }

  it should "create a parser that succeeds with an empty Option" in {
    point.maybe.parse("a123") should matchPattern { case (Success(None), "a123") => }
  }

  "many" should "apply the parser as many times as possible until it produces a failure" in {
    point.many.parse("123a456") should matchPattern { case (Success(List(1, 2, 3)), "a456") => }
  }

  it should "apply the parser as many times as possible until the end of the input is reached" in {
    point.many.parse("123") should matchPattern { case (Success(List(1, 2, 3)), "") => }
  }

  it should "not fail on an empty input, but return an empty list instead" in {
    point.many.parse("") should matchPattern { case (Success(Nil), "") => }
  }

  it should "not fail on a corrupt first input, but return an empty list instead" in {
    point.many.parse("a") should matchPattern { case (Success(Nil), "a") => }
  }

  "atLeastOnce" should "apply the parser as many times as possible, but at least, once until it produces a failure" in {
    point.atLeastOnce.parse("123a456") should matchPattern { case (Success(List(1, 2, 3)), "a456") => }
  }

  it should "apply the parser as many times as possible, but at least once, until the end of the input is reached" in {
    point.atLeastOnce.parse("123") should matchPattern { case (Success(List(1, 2, 3)), "") => }
  }

  it should "fail on an empty input" in {
    point.atLeastOnce.parse("") should matchPattern { case (Failure(`emptyError`), "") => }
  }

  it should "fail on a corrupt first input" in {
    point.atLeastOnce.parse("a") should matchPattern { case (Failure(_: NumberFormatException), "") => }
  }

  "takeWhile" should "apply the parser as many times as possible, as long as the predicate holds" in {
    point.takeWhile(_ % 2 == 0).parse("2467") should matchPattern { case (Success(List(2, 4, 6)), "7") => }
  }

  it should "apply the parser until it runs out of input, provided the predicate holds for all input" in {
    point.takeWhile(_ % 2 == 0).parse("2468") should matchPattern { case (Success(List(2, 4, 6, 8)), "") => }
  }

  it should "apply the parser until it reaches an input it cannot parse" in {
    point.takeWhile(_ % 2 == 0).parse("246a8") should matchPattern { case (Success(List(2, 4, 6)), "a8") => }
  }

  "takeUntil" should "apply the parser as many times as possible, as long as the predicate holds" in {
    point.takeUntil(_ % 2 != 0).parse("2467") should matchPattern { case (Success(List(2, 4, 6)), "7") => }
  }

  it should "apply the parser until it runs out of input, provided the predicate holds for all input" in {
    point.takeUntil(_ % 2 != 0).parse("2468") should matchPattern { case (Success(List(2, 4, 6, 8)), "") => }
  }

  it should "apply the parser until it reaches an input it cannot parse" in {
    point.takeUntil(_ % 2 != 0).parse("246a8") should matchPattern { case (Success(List(2, 4, 6)), "a8") => }
  }

  "separatedBy" should "apply both the parser and the separater parser multiple times after each other with the separator as the last one before we run out of input" in {
    point.separatedBy(point.satisfy(_ % 2 == 0))
      .parse("123456") should matchPattern { case (Success(List(1, 3, 5)), "6") =>  }
  }

  it should "apply both the parser and the separator parser multiple times with the parser as the last one before we run out of input" in {
    point.separatedBy(point.satisfy(_ % 2 == 0))
      .parse("1234567") should matchPattern { case (Success(List(1, 3, 5, 7)), "") => }
  }

  it should "apply both the parser and the separator parser multiple times until the separator parser fails" in {
    point.separatedBy(point.satisfy(_ % 2 == 0))
      .parse("123457") should matchPattern { case (Success(List(1, 3, 5)), "7") => }
  }

  it should "apply both the parser and the separator parser multiple times until the parser fails" in {
    point.separatedBy(point.satisfy(_ % 2 == 0))
      .parse("1234a6") should matchPattern { case (Success(List(1, 3)), "4a6") => }
  }

  it should "return an empty output when given an empty input" in {
    point.separatedBy(point.satisfy(_ % 2 == 0))
      .parse("") should matchPattern { case (Success(Nil), "") => }
  }

  it should "return an empty output when given an input that fails immediately" in {
    point.separatedBy(point.satisfy(_ % 2 == 0))
      .parse("a2345") should matchPattern { case (Success(Nil), "a2345") => }
  }

  "separatedBy1" should "apply both the parser and the separater parser multiple times after each other with the separator as the last one before we run out of input" in {
    point.separatedBy1(point.satisfy(_ % 2 == 0))
      .parse("123456") should matchPattern { case (Success(List(1, 3, 5)), "6") => }
  }

  it should "apply both the parser and the separator parser multiple times with the parser as the last one before we run out of input" in {
    point.separatedBy1(point.satisfy(_ % 2 == 0))
      .parse("1234567") should matchPattern { case (Success(List(1, 3, 5, 7)), "") => }
  }

  it should "apply both the parser and the separator parser multiple times until the separator parser fails" in {
    point.separatedBy1(point.satisfy(_ % 2 == 0))
      .parse("123457") should matchPattern { case (Success(List(1, 3, 5)), "7") => }
  }

  it should "apply both the parser and the separator parser multiple times until the parser fails" in {
    point.separatedBy1(point.satisfy(_ % 2 == 0))
      .parse("1234a6") should matchPattern { case (Success(List(1, 3)), "4a6") => }
  }

  it should "fail when given an empty input" in {
    point.separatedBy1(point.satisfy(_ % 2 == 0))
      .parse("") should matchPattern { case (Failure(`emptyError`), "") => }
  }

  it should "fail when given an input that fails immediately" in {
    point.separatedBy1(point.satisfy(_ % 2 == 0))
      .parse("a2345") should matchPattern { case (Failure(_: NumberFormatException), "2345") => }
  }

  "skipMany" should "discard the input as long as it satisfies the parser" in {
    point.satisfy(_ % 2 == 0)
      .skipMany
      .parse("24689123") should matchPattern { case (Success(_), "9123") => }
  }

  it should "return Unit if the parser fails immediately" in {
    point.skipMany.parse("a123") should matchPattern { case (Success(_), "a123") => }
  }

  it should "return Unit if the parser fails eventually" in {
    point.skipMany.parse("123a456") should matchPattern { case (Success(_), "a456") => }
  }
}
