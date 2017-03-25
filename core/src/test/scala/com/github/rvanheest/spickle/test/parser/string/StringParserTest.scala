package com.github.rvanheest.spickle.test.parser.string

import org.scalatest.{FlatSpec, Inside, Matchers}
import com.github.rvanheest.spickle.parser.string.StringParser._

import scala.util.{Failure, Success}

class StringParserTest extends FlatSpec with Matchers with Inside {

  "item" should "take the first character from the input string" in {
    inside(item.parse("abc")) {
      case (Success(c), s) =>
        c shouldBe 'a'
        s shouldBe "bc"
    }
  }

  it should "return a failure with a NoSuchElementException when the input string is empty" in {
    inside(item.parse("")) {
      case (Failure(e), s) =>
        e shouldBe a[NoSuchElementException]
        s shouldBe empty
    }
  }

  "digit" should "only parse the first character if it is a digit" in {
    inside(digit.parse("1abc")) {
      case (Success(n), s) =>
        n shouldBe '1'
        s shouldBe "abc"
    }
  }

  "number" should "parse multiple digits as long as they're digits and concat them" in {
    inside(number.parse("1234abc")) {
      case (Success(n), s) =>
        n shouldBe "1234"
        s shouldBe "abc"
    }
  }

  "lower" should "parse a single character from the input, provided it is a lowercase letter" in {
    inside(lower.parse("a12")) {
      case (Success(c), s) =>
        c shouldBe 'a'
        s shouldBe "12"
    }
  }

  "upper" should "parse a single character from the input, provided it is an UPPERCASE letter" in {
    inside(upper.parse("A12")) {
      case (Success(c), s) =>
        c shouldBe 'A'
        s shouldBe "12"
    }
  }

  "letter" should "parse a single character from the input, provided it is a lowercase letter" in {
    inside(letter.parse("a12")) {
      case (Success(c), s) =>
        c shouldBe 'a'
        s shouldBe "12"
    }
  }

  it should "parse a single character from the input, provided it is an UPPERCASE letter" in {
    inside(letter.parse("A12")) {
      case (Success(c), s) =>
        c shouldBe 'A'
        s shouldBe "12"
    }
  }

  "alphanum" should "parse a single character from the input, provided it is a lowercase letter" in {
    inside(alphanum.parse("a12")) {
      case (Success(c), s) =>
        c shouldBe 'a'
        s shouldBe "12"
    }
  }

  it should "parse a single character from the input, provided it is an UPPERCASE letter" in {
    inside(alphanum.parse("A12")) {
      case (Success(c), s) =>
        c shouldBe 'A'
        s shouldBe "12"
    }
  }

  it should "parse a single character from the input, provided it is a digit" in {
    inside(alphanum.parse("1AB")) {
      case (Success(n), s) =>
        n shouldBe '1'
        s shouldBe "AB"
    }
  }

  "char" should "parse a single character from the input, provided it is the specified digit" in {
    inside(char('a').parse("abc")) {
      case (Success(c), s) =>
        c shouldBe 'a'
        s shouldBe "bc"
    }
  }

  "space" should "parse a single character from the input, provided it is a space" in {
    inside(space.parse(" abc")) {
      case (Success(c), s) =>
        c shouldBe ' '
        s shouldBe "abc"
    }
  }

  "spaces" should "skip spaces until it reaches a non-space character" in {
    inside(spaces.parse("   abc")) {
      case (Success(_), s) =>
        s shouldBe "abc"
    }
  }

  "string" should "consume a specified String from the input and return this string if it matches" in {
    val input = "abc"
    inside(string(input).parse("abcdef")) {
      case (Success(res), s) =>
        res shouldBe input
        s shouldBe "def"
    }
  }

  it should "consume nothing if the empty string is given as input" in {
    val input = ""
    inside(string(input).parse("abcdef")) {
      case (Success(res), s) =>
        res shouldBe input
        s shouldBe "abcdef"
    }
  }
}
