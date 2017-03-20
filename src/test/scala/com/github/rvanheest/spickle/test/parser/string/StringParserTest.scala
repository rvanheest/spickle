package com.github.rvanheest.spickle.test.parser.string

import org.scalatest.{FlatSpec, Inside, Matchers}
import com.github.rvanheest.spickle.parser.string.StringParser._

import scala.util.{Failure, Success}

class StringParserTest extends FlatSpec with Matchers with Inside {

  "item" should "take the first character from the input string" in {
    inside(item.run("abc")) {
      case (Success(c), s) =>
        c shouldBe 'a'
        s shouldBe "bc"
    }
  }

  it should "return a failure with a NoSuchElementException when the input string is empty" in {
    inside(item.run("")) {
      case (Failure(e), s) =>
        e shouldBe a[NoSuchElementException]
        s shouldBe empty
    }
  }

  "digit" should "only parse the first character if it is a digit" in {
    inside(digit.run("1abc")) {
      case (Success(c), s) =>
        c shouldBe '1'
        s shouldBe "abc"
    }
  }

  "number" should "parse multiple digits as long as they're digits and concat them" in {
    inside(number.run("1234abc")) {
      case (Success(n), s) =>
        n shouldBe "1234"
        s shouldBe "abc"
    }
  }

  // TODO continue with lower
}
