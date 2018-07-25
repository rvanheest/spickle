package com.github.rvanheest.spickle.example.string

import com.github.rvanheest.spickle.parser.string.StringParser._

object ExpressionParser extends App {

  def expr: StringParser[Int] = (for {
    a <- term
    _ <- space.maybe
    _ <- char('+')
    _ <- space.maybe
    b <- term
  } yield a + b) orElse term

  def term: StringParser[Int] = (for {
    a <- factor
    _ <- space.maybe
    _ <- char('*')
    _ <- space.maybe
    b <- factor
  } yield a * b) orElse factor

  def factor: StringParser[Int] = number.toInt <|> (for {
    _ <- char('(')
    a <- expr
    _ <- char(')')
  } yield a)

  println("1 + 1 = " + expr.eval("1 + 1").get)
  println("2 * 3 + 1 = " + expr.eval("2 * 3 + 1").get)
  println("(2 + 3) * 4 = " + expr.eval("(2 + 3) * 4").get)
  println("2 + 3 * 4 = " + expr.eval("2 + 3 * 4").get)
  println("(2 * 3) + (2 + 7) = " + expr.eval("(2 * 3) + (2 + 7)").get)
}
