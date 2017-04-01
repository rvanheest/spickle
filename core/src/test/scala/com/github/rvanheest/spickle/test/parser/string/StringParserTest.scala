package com.github.rvanheest.spickle.test.parser.string

import com.github.rvanheest.spickle.parser.ParserFailedException
import com.github.rvanheest.spickle.parser.string.StringParser._
import org.scalatest.{ FlatSpec, Matchers }

import scala.util.{ Failure, Success }

class StringParserTest extends FlatSpec with Matchers {

  "item" should "take the first character from the input string" in {
    item.parse("abc") should matchPattern { case (Success('a'), "bc") => }
  }

  it should "return a failure with a NoSuchElementException when the input string is empty" in {
    item.parse("") should matchPattern { case (Failure(_: NoSuchElementException), "") => }
  }

  "digit" should "only parse the first character if it is a digit" in {
    digit.parse("1abc") should matchPattern { case (Success('1'), "abc") => }
  }

  it should "fail if the first character is not a digit" in {
    val expectedMsg = "input 'a' is not a digit"
    digit.parse("abc") should matchPattern { case (Failure(ParserFailedException(`expectedMsg`)), "bc") => }
  }

  "number" should "parse multiple digits as long as they're digits and concat them" in {
    number.parse("1234abc") should matchPattern { case (Success("1234"), "abc") => }
  }

  it should "fail if the first character is not a digit" in {
    val expectedMsg = "input 'a' is not a digit"
    number.parse("abc") should matchPattern { case (Failure(ParserFailedException(`expectedMsg`)), "bc") => }
  }

  "lower" should "parse a single character from the input, provided it is a lowercase character" in {
    lower.parse("a12") should matchPattern { case (Success('a'), "12") => }
  }

  it should "fail if the first character is not a lowercase character" in {
    val expectedMsg = "input 'A' is not a lowercase character"
    lower.parse("Abc") should matchPattern { case (Failure(ParserFailedException(`expectedMsg`)), "bc") => }
  }

  "upper" should "parse a single character from the input, provided it is an UPPERCASE letter" in {
    upper.parse("A12") should matchPattern { case (Success('A'), "12") => }
  }

  it should "fail if the first character is not an uppercase character" in {
    val expectedMsg = "input 'a' is not an uppercase character"
    upper.parse("aBC") should matchPattern { case (Failure(ParserFailedException(`expectedMsg`)), "BC") => }
  }

  "letter" should "parse a single character from the input, provided it is a lowercase letter" in {
    letter.parse("a12") should matchPattern { case (Success('a'), "12") => }
  }

  it should "parse a single character from the input, provided it is an UPPERCASE letter" in {
    letter.parse("A12") should matchPattern { case (Success('A'), "12") => }
  }

  it should "fail if the first character is not a letter" in {
    val expectedMsg = "input '1' is not a letter"
    letter.parse("1Bc") should matchPattern { case (Failure(ParserFailedException(`expectedMsg`)), "Bc") => }
  }

  "alphanum" should "parse a single character from the input, provided it is a lowercase letter" in {
    alphanum.parse("a12") should matchPattern { case (Success('a'), "12") => }
  }

  it should "parse a single character from the input, provided it is an UPPERCASE letter" in {
    alphanum.parse("A12") should matchPattern { case (Success('A'), "12") => }
  }

  it should "parse a single character from the input, provided it is a digit" in {
    alphanum.parse("1AB") should matchPattern { case (Success('1'), "AB") => }
  }

  it should "fail if the first character is not an alphanumeric character" in {
    val expectedMsg = "input '#' is not an alphanumeric character"
    alphanum.parse("#bc") should matchPattern { case (Failure(ParserFailedException(`expectedMsg`)), "bc") => }
  }

  "char" should "parse a single character from the input, provided it is the specified character" in {
    char('a').parse("abc") should matchPattern { case (Success('a'), "bc") => }
  }

  it should "fail if the first character is not the specified character" in {
    val expectedMsg = "input 'b' is not equal to 'a'"
    char('a').parse("bcd") should matchPattern { case (Failure(ParserFailedException(`expectedMsg`)), "cd") => }
  }

  "space" should "parse a single character from the input, provided it is a space" in {
    space.parse(" abc") should matchPattern { case (Success(' '), "abc") => }
  }

  "spaces" should "skip spaces until it reaches a non-space character" in {
    spaces.parse("   abc") should matchPattern { case (Success(_), "abc") => }
  }

  "string" should "consume a specified String from the input and return this string if it matches" in {
    val input = "abc"
    string(input).parse("abcdef") should matchPattern { case (Success(`input`), "def") => }
  }

  it should "consume nothing if the empty string is given as input" in {
    val input = ""
    string(input).parse("abcdef") should matchPattern { case (Success(`input`), "abcdef") => }
  }
}
