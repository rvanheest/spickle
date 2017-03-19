package com.github.rvanheest.spickle.test.parser

import com.github.rvanheest.spickle.parser.Parser
import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.language.postfixOps
import scala.util.{Failure, Success}

class ParserTest extends FlatSpec with Matchers with Inside {

  private def successParser = new Parser[String, Int](s => (Success(1), s"hello $s"))
  private def failureParser(msg: String) = new Parser[String, Int](s => (Failure(new Exception(msg)), s))

  "run" should "apply the function given to the parser and return both the result and the remaining state" in {
    inside(successParser.run("world")) {
      case (Success(x), s) =>
        x shouldBe 1
        s shouldBe "hello world"
    }
  }

  "eval" should "apply the function given to the parser and return only the result" in {
    inside(successParser.eval("world")) {
      case Success(x) => x shouldBe 1
    }
  }

  "execute" should "apply the function given to the parser and return only the remaining state" in {
    successParser.execute("world") shouldBe "hello world"
  }

  "orElse" should "only run the first parser if it returns a success" in {
    var notVisited = true
    val p1 = successParser
    val p2 = Parser[String, Int](s => { notVisited = false; (Failure(new Exception), s) })

    inside(p1.orElse(p2).run("world")) {
      case (Success(x), s) =>
        x shouldBe 1
        s shouldBe "hello world"
        notVisited shouldBe true
    }
  }

  it should "run the second parser if the first parser returns a failure" in {
    val p1 = failureParser("ex1")
    val p2 = failureParser("ex2")

    inside(p1.orElse(p2).run("world")) {
      case (Failure(e), s) =>
        e.getMessage shouldBe "ex2"
        s shouldBe "world"
    }
  }

  "map" should "apply the function in map when the parser returns a success" in {
    inside(successParser.map(2 *).run("world")) {
      case (Success(x), s) =>
        x shouldBe 2
        s shouldBe "hello world"
    }
  }

  it should "not apply the function in map when the parser returns a failure" in {
    var notVisited = true
    inside(failureParser("error").map(i => { notVisited = false; i * 2 }).run("world")) {
      case (Failure(_), s) =>
        s shouldBe "world"
        notVisited shouldBe true
    }
  }

  "flatMap" should "apply the function in flatMap when the parser returns a success" in {
    inside(successParser.flatMap(i => Parser[String, Int](s => (Success(i + 2), s"$s!"))).run("world")) {
      case (Success(x), s) =>
        x shouldBe 3
        s shouldBe "hello world!"
    }
  }

  it should "apply the function in flatMap when the first parser returns a success even if the inner parser produces a failure" in {
    inside(failureParser("ex1").flatMap(_ => failureParser("ex2")).run("world")) {
      case (Failure(e), s) =>
        e.getMessage shouldBe "ex1"
        s shouldBe "world"
    }
  }

  it should "not apply the function in flatMap when the first parser returns a failure" in {
    var notVisited = true
    inside(failureParser("ex1").flatMap(i => { notVisited = false; successParser.map(i +) }).run("world")) {
      case (Failure(e), s) =>
        e.getMessage shouldBe "ex1"
        s shouldBe "world"
        notVisited shouldBe true
    }
  }

  "transform" should "apply the function in transform when the parser returns a success" in {
    inside(successParser.transform((i, s) => (Success(i + 1), s + "!")).run("world")) {
      case (Success(i), s) =>
        i shouldBe 2
        s shouldBe "hello world!"
    }
  }

  it should "not apply the function in transform when the parser returns a failure" in {
    var notVisited = true
    inside(failureParser("error").transform((i, s) => { notVisited = false; (Success(i + 1), "hello " + s) }).run("world")) {
      case (Failure(_), s) =>
        s shouldBe "world"
        notVisited shouldBe true
    }
  }

  "satisfy" should "produce a succeeding parser if the predicate succeeds" in {
    inside(successParser.map(_ => 2).satisfy(_ % 2 == 0).run("world")) {
      case (Success(i), s) =>
        i shouldBe 2
        s shouldBe "hello world"
    }
  }

  it should "produce a failing parser if the predicate fails" in {
    inside(successParser.map(_ => 3).satisfy(_ % 2 == 0).run("world")) {
      case (Failure(e), s) =>
        e.getMessage shouldBe "empty parser"
        s shouldBe "hello world"
    }
  }

  it should "not apply the predicate if the parser fails" in {
    var notVisited = true
    inside(failureParser("error").satisfy(_ => { notVisited = false; true }).run("world")) {
      case (Failure(e), s) =>
        e.getMessage shouldBe "error"
        s shouldBe "world"
        notVisited shouldBe true
    }
  }

  "noneOf" should "create a parser that succeeds when the value is not in the given list" in {
    inside(successParser.noneOf(List(2, 3, 4)).run("world")) {
      case (Success(i), s) =>
        i shouldBe 1
        s shouldBe "hello world"
    }
  }

  it should "create a parser that fails when the value is in the given list" in {
    inside(successParser.noneOf(List(0, 1, 2)).run("world")) {
      case (Failure(e), s) =>
        e.getMessage shouldBe "empty parser"
        s shouldBe "hello world"
    }
  }

  // TODO continue testing at `maybe`
}
