package com.github.rvanheest.spickle.test.parser

import com.github.rvanheest.spickle.parser.Parser
import org.scalatest.{ FlatSpec, Inside, Matchers }

import scala.language.postfixOps
import scala.util.{ Failure, Success, Try }

class ParserTest extends FlatSpec with Matchers with Inside {

  val emptyError = new NoSuchElementException("you're trying to parse a character in an empty String")

  type TestParser = Parser[String, Int]

  def point: TestParser = new TestParser(s => s.toList match {
    case Nil => (Failure(emptyError), "")
    case p :: ps => (Try(p.toString.toInt), ps.mkString)
  })

  "run" should "apply the function given to the parser and return both the result and the remaining state" in {
    inside(point.run("123")) {
      case (Success(x), s) =>
        x shouldBe 1
        s shouldBe "23"
    }
  }

  "eval" should "apply the function given to the parser and return only the result" in {
    inside(point.eval("123")) {
      case Success(x) => x shouldBe 1
    }
  }

  "execute" should "apply the function given to the parser and return only the remaining state" in {
    point.execute("123") shouldBe "23"
  }

  "orElse" should "only run the first parser if it returns a success" in {
    var notVisited = true
    val p1 = point
    val p2 = Parser[String, Int](s => { notVisited = false; (Failure(new Exception), s) })

    inside(p1.orElse(p2).run("123")) {
      case (Success(x), s) =>
        x shouldBe 1
        s shouldBe "23"
        notVisited shouldBe true
    }
  }

  it should "run the second parser if the first parser returns a failure" in {
    val p1 = point
    val p2 = Parser.failure[String, Int](new Exception("ex"))

    inside(p1.orElse(p2).run("a123")) {
      case (Failure(e), s) =>
        e.getMessage shouldBe "ex"
        s shouldBe "a123"
    }
  }

  "map" should "apply the function in map when the parser returns a success" in {
    inside(point.map(2 *).run("123")) {
      case (Success(x), s) =>
        x shouldBe 2
        s shouldBe "23"
    }
  }

  it should "not apply the function in map when the parser returns a failure" in {
    var notVisited = true
    inside(point.map(i => { notVisited = false; i * 2 }).run("a123")) {
      case (Failure(e), s) =>
        e shouldBe a[NumberFormatException]
        s shouldBe "123"
        notVisited shouldBe true
    }
  }

  "flatMap" should "apply the function in flatMap when the parser returns a success" in {
    inside(point.flatMap(i => point.map(10 * i +)).run("123")) {
      case (Success(x), s) =>
        x shouldBe 12
        s shouldBe "3"
    }
  }

  it should "apply the function in flatMap when the first parser returns a success even if the inner parser produces a failure" in {
    inside(point.flatMap(_ => Parser.failure[String, Int](new Exception("error"))).run("123")) {
      case (Failure(e), s) =>
        e.getMessage shouldBe "error"
        s shouldBe "23"
    }
  }

  it should "not apply the function in flatMap when the first parser returns a failure" in {
    var notVisited = true
    inside(point.flatMap(i => { notVisited = false; point.map(i +) }).run("a123")) {
      case (Failure(e), s) =>
        e shouldBe a[NumberFormatException]
        s shouldBe "123"
        notVisited shouldBe true
    }
  }

  "transform" should "apply the function in transform when the parser returns a success" in {
    inside(point.transform((i, s) => (if (i == 1) Success(i + 1) else Failure(new Exception("error!")), i + s)).run("123")) {
      case (Success(i), s) =>
        i shouldBe 2
        s shouldBe "123"
    }
  }

  it should "not apply the function in transform when the parser returns a failure" in {
    inside(point.transform((i, s) => (if (i == 1) Success(i + 1) else Failure(new Exception("error!")), i + s)).run("234")) {
      case (Failure(e), s) =>
        e.getMessage shouldBe "error!"
        s shouldBe "234"
    }
  }

  "satisfy" should "produce a succeeding parser if the predicate succeeds" in {
    inside(point.satisfy(_ % 2 == 1).run("123")) {
      case (Success(i), s) =>
        i shouldBe 1
        s shouldBe "23"
    }
  }

  it should "produce a failing parser if the predicate fails" in {
    inside(point.satisfy(_ % 2 == 0).run("123")) {
      case (Failure(e), s) =>
        e shouldBe a[NoSuchElementException]
        e.getMessage shouldBe "empty parser"
        s shouldBe "23"
    }
  }

  it should "not apply the predicate if the parser fails" in {
    var notVisited = true
    inside(point.satisfy(i => { notVisited = false; i % 2 == 0 }).run("")) {
      case (Failure(e), s) =>
        e shouldBe emptyError
        s shouldBe empty
        notVisited shouldBe true
    }
  }

  "noneOf" should "create a parser that succeeds when the value is not in the given list" in {
    inside(point.noneOf(List(2, 3, 4)).run("123")) {
      case (Success(i), s) =>
        i shouldBe 1
        s shouldBe "23"
    }
  }

  it should "create a parser that fails when the value is in the given list" in {
    inside(point.noneOf(List(0, 1, 2)).run("123")) {
      case (Failure(e), s) =>
        e shouldBe a[NoSuchElementException]
        e.getMessage shouldBe "empty parser"
        s shouldBe "23"
    }
  }

  "maybe" should "create a parser that succeeds with the value wrapped in an Option" in {
    inside(point.maybe.run("123")) {
      case (Success(opt), s) =>
        opt should contain (1)
        s shouldBe "23"
    }
  }

  it should "create a parser that succeeds with an empty Option" in {
    inside(point.maybe.run("a123")) {
      case (Success(opt), s) =>
        opt shouldBe empty
        s shouldBe "a123"
    }
  }

  "many" should "apply the parser as many times as possible until it produces a failure" in {
    inside(point.many.run("123a456")) {
      case (Success(is), s) =>
        is should (have size 3 and contain inOrderOnly (1, 2, 3))
        s shouldBe "a456"
    }
  }

  it should "apply the parser as many times as possible until the end of the input is reached" in {
    inside(point.many.run("123")) {
      case (Success(is), s) =>
        is should (have size 3 and contain inOrderOnly (1, 2, 3))
        s shouldBe empty
    }
  }

  it should "not fail on an empty input, but return an empty list instead" in {
    inside(point.many.run("")) {
      case (Success(is), s) =>
        is shouldBe empty
        s shouldBe empty
    }
  }

  it should "not fail on a corrupt first input, but return an empty list instead" in {
    inside(point.many.run("a")) {
      case (Success(is), s) =>
        is shouldBe empty
        s shouldBe "a"
    }
  }

  "atLeastOnce" should "apply the parser as many times as possible, but at least, once until it produces a failure" in {
    inside(point.atLeastOnce.run("123a456")) {
      case (Success(is), s) =>
        is should (have size 3 and contain inOrderOnly (1, 2, 3))
        s shouldBe "a456"
    }
  }

  it should "apply the parser as many times as possible, but at least once, until the end of the input is reached" in {
    inside(point.atLeastOnce.run("123")) {
      case (Success(is), s) =>
        is should (have size 3 and contain inOrderOnly (1, 2, 3))
        s shouldBe empty
    }
  }

  it should "fail on an empty input" in {
    inside(point.atLeastOnce.run("")) {
      case (Failure(e), s) =>
        e shouldBe emptyError
        s shouldBe empty
    }
  }

  it should "fail on a corrupt first input" in {
    inside(point.atLeastOnce.run("a")) {
      case (Failure(e), s) =>
        e shouldBe a[NumberFormatException]
        s shouldBe empty
    }
  }

  "takeWhile" should "apply the parser as many times as possible, as long as the predicate holds" in {
    inside(point.takeWhile(_ % 2 == 0).run("2467")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(2, 4, 6)
        s shouldBe "7"
    }
  }

  it should "apply the parser until it runs out of input, provided the predicate holds for all input" in {
    inside(point.takeWhile(_ % 2 == 0).run("2468")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(2, 4, 6, 8)
        s shouldBe empty
    }
  }

  it should "apply the parser untilit reaches an input it cannot parse" in {
    inside(point.takeWhile(_ % 2 == 0).run("246a8")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(2, 4, 6)
        s shouldBe "a8"
    }
  }

  "takeUntil" should "apply the parser as many times as possible, as long as the predicate holds" in {
    inside(point.takeWhile(_ % 2 != 1).run("2467")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(2, 4, 6)
        s shouldBe "7"
    }
  }

  it should "apply the parser until it runs out of input, provided the predicate holds for all input" in {
    inside(point.takeWhile(_ % 2 != 1).run("2468")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(2, 4, 6, 8)
        s shouldBe empty
    }
  }

  it should "apply the parser untilit reaches an input it cannot parse" in {
    inside(point.takeWhile(_ % 2 != 1).run("246a8")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(2, 4, 6)
        s shouldBe "a8"
    }
  }

  "separatedBy" should "apply both the parser and the separater parser multiple times after each other with the separator as the last one before we run out of input" in {
    inside(point.separatedBy(point.satisfy(_ % 2 == 0)).run("123456")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(1, 3, 5)
        s shouldBe "6"
    }
  }

  it should "apply both the parser and the separator parser multiple times with the parser as the last one before we run out of input" in {
    inside(point.separatedBy(point.satisfy(_ % 2 == 0)).run("1234567")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(1, 3, 5, 7)
        s shouldBe empty
    }
  }

  it should "apply both the parser and the separator parser multiple times until the separator parser fails" in {
    inside(point.separatedBy(point.satisfy(_ % 2 == 0)).run("123457")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(1, 3, 5)
        s shouldBe "7"
    }
  }

  it should "apply both the parser and the separator parser multiple times until the parser fails" in {
    inside(point.separatedBy(point.satisfy(_ % 2 == 0)).run("1234a6")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(1, 3)
        s shouldBe "4a6"
    }
  }

  it should "return an empty output when given an empty input" in {
    inside(point.separatedBy(point.satisfy(_ % 2 == 0)).run("")) {
      case (Success(is), s) =>
        is shouldBe empty
        s shouldBe empty
    }
  }

  it should "return an empty output when given an input that fails immediately" in {
    inside(point.separatedBy(point.satisfy(_ % 2 == 0)).run("a2345")) {
      case (Success(is), s) =>
        is shouldBe empty
        s shouldBe "a2345"
    }
  }

  "separatedBy1" should "apply both the parser and the separater parser multiple times after each other with the separator as the last one before we run out of input" in {
    inside(point.separatedBy1(point.satisfy(_ % 2 == 0)).run("123456")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(1, 3, 5)
        s shouldBe "6"
    }
  }

  it should "apply both the parser and the separator parser multiple times with the parser as the last one before we run out of input" in {
    inside(point.separatedBy1(point.satisfy(_ % 2 == 0)).run("1234567")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(1, 3, 5, 7)
        s shouldBe empty
    }
  }

  it should "apply both the parser and the separator parser multiple times until the separator parser fails" in {
    inside(point.separatedBy1(point.satisfy(_ % 2 == 0)).run("123457")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(1, 3, 5)
        s shouldBe "7"
    }
  }

  it should "apply both the parser and the separator parser multiple times until the parser fails" in {
    inside(point.separatedBy1(point.satisfy(_ % 2 == 0)).run("1234a6")) {
      case (Success(is), s) =>
        is should contain inOrderOnly(1, 3)
        s shouldBe "4a6"
    }
  }

  it should "fail when given an empty input" in {
    inside(point.separatedBy1(point.satisfy(_ % 2 == 0)).run("")) {
      case (Failure(e), s) =>
        e shouldBe emptyError
        s shouldBe empty
    }
  }

  it should "fail when given an input that fails immediately" in {
    inside(point.separatedBy1(point.satisfy(_ % 2 == 0)).run("a2345")) {
      case (Failure(e), s) =>
        e shouldBe a[NumberFormatException]
        s shouldBe "2345"
    }
  }

  "skipMany" should "discard the input as long as it satisfies the parser" in {
    inside(point.satisfy(_ % 2 == 0).skipMany.run("24689123")) {
      case (Success(_), s) => s shouldBe "9123"
    }
  }

  it should "return Unit if the parser fails immediately" in {
    inside(point.skipMany.run("a123")) {
      case (Success(_), s) => s shouldBe "a123"
    }
  }

  it should "return Unit if the parser fails eventually" in {
    inside(point.skipMany.run("123a456")) {
      case (Success(_), s) => s shouldBe "a456"
    }
  }
}