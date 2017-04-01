package com.github.rvanheest.spickle.test.pickle.string

import com.github.rvanheest.spickle.pickle.PickleFailedException
import com.github.rvanheest.spickle.pickle.string.StringPickle._
import org.scalatest.{ FlatSpec, Matchers }

import scala.util.{ Failure, Success }

class StringPickleTest extends FlatSpec with Matchers {

  "item" should "prepend the character to the string" in {
    item.pickle('a', "bc") should matchPattern { case Success("abc") => }
  }

  "digit" should "pickle the character if it is a digit" in {
    digit.pickle('1', "abc") should matchPattern { case Success("1abc") => }
  }

  it should "fail if the character is not a digit" in {
    val expectedMsg = "input 'a' is not a digit"
    digit.pickle('a', "bc") should matchPattern { case Failure(PickleFailedException(`expectedMsg`)) => }
  }

  "number" should "pickle multiple digits if they're all digits" in {
    number.pickle("1234", "abc") should matchPattern { case Success("1234abc") => }
  }

  it should "fail if the digits contain a non-digit character" in {
    val expectedMsg = "input 'a' is not a digit"
    number.pickle("1234a", "bc") should matchPattern { case Failure(PickleFailedException(`expectedMsg`)) => }
  }

  "lower" should "pickle the character if it is a lowercase character" in {
    lower.pickle('a', "12") should matchPattern { case Success("a12") => }
  }

  it should "fail if the character is not a lowercase character" in {
    val expectedMsg = "input 'A' is not a lowercase character"
    lower.pickle('A', "bc") should matchPattern { case Failure(PickleFailedException(`expectedMsg`)) => }
  }

  "upper" should "pickle the character if it is an UPPERCASE letter" in {
    upper.pickle('A', "12") should matchPattern { case Success("A12") => }
  }

  it should "fail if the character is not an UPPERCASE character" in {
    val expectedMsg = "input 'a' is not an uppercase character"
    upper.pickle('a', "BC") should matchPattern { case Failure(PickleFailedException(`expectedMsg`)) => }
  }

  "letter" should "pickle the character if it is a lowercase letter" in {
    letter.pickle('a', "12") should matchPattern { case Success("a12") => }
  }

  it should "pickle the character if it is an UPPERCASE letter" in {
    letter.pickle('A', "12") should matchPattern { case Success("A12") => }
  }

  it should "fail if the character is not a letter" in {
    val expectedMsg = "input '1' is not a letter"
    letter.pickle('1', "Bc") should matchPattern { case Failure(PickleFailedException(`expectedMsg`)) => }
  }

  "alphanum" should "pickle the character if it is a lowercase letter" in {
    alphanum.pickle('a', "12") should matchPattern { case Success("a12") => }
  }

  it should "pickle the character if it is an UPPERCASE letter" in {
    alphanum.pickle('A', "12") should matchPattern { case Success("A12") => }
  }

  it should "pickle the character if it is a digit" in {
    alphanum.pickle('1', "AB") should matchPattern { case Success("1AB") => }
  }

  it should "fail if the character is not an alphanumeric character" in {
    val expectedMsg = "input '#' is not an alphanumeric character"
    alphanum.pickle('#', "bc") should matchPattern { case Failure(PickleFailedException(`expectedMsg`)) => }
  }

  "char" should "pickle the character if it is the specified character" in {
    char('a').pickle('a', "bc") should matchPattern { case Success("abc") => }
  }

  it should "fail if the character is not the specified character" in {
    val expectedMsg = "input 'b' is not equal to 'a'"
    char('a').pickle('b', "cd") should matchPattern { case Failure(PickleFailedException(`expectedMsg`)) => }
  }

  "space" should "pickle the character from the input, provided it is a space" in {
    space.pickle(' ' , "abc") should matchPattern { case Success(" abc") => }
  }

  "string" should "pickle the input string if it matches the given string" in {
    string("abc").pickle("abc", "def") should matchPattern { case Success("abcdef") => }
  }

  it should "consume nothing if the empty string is given as input" in {
    string("").pickle("", "abcdef") should matchPattern { case Success("abcdef") => }
  }

  it should "fail if the input string does not match the given string" in {
    string("abc").pickle("ab", "def") should matchPattern { case Failure(_: NoSuchElementException) => }
  }
}
