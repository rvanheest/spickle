package com.github.rvanheest.spickle.example.string

import com.github.rvanheest.spickle.parser.string.StringParser._

object SimpleStringParsers extends App {

  val itemParser1 = item
  // should parse the first character and leave the rest of the input untouched
  println(itemParser1.parse("foo"))

  val itemParser2 = for {
    first <- item
    second <- item
    third <- item
  } yield List(first, second, third).mkString
  // should parse all three characters of the input and concat them into a String
  println(itemParser2.parse("foo"))

  val digitParser = digit
  // should parse the character because it is a digit
  println(digitParser.parse("5"))
  // should not parse the character because it isn't a digit; should fail instead
  println(digitParser.parse("a"))

  val numberParser = number
  // should parse the whole input because all characters are digits
  println(numberParser.parse("345"))
  // should parse only the first two characters, because the third is not a digit
  println(numberParser.parse("45a6"))

  val patternParser = for {
    s1 <- number
    s2 <- letter.atLeastOnce
    s3 <- number
  } yield (s1, s2.mkString("[", "-", "]"), s3)
  // this parser specifies that the input must first have a number (0-n digits),
  // followed by any number of letters, followed by another number
  println(patternParser.parse("45abc6"))
}
