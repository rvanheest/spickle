package com.github.rvanheest.spickle.serializer.string

import com.github.rvanheest.spickle.serializer.Serializer
import com.github.rvanheest.spickle.serializer.Serializer.from

import scala.language.postfixOps
import scala.util.Try

object StringSerializer {

  type StringSerializer[A] = Serializer[String, A]

  def item: StringSerializer[Char] = {
    Serializer(
      (c, s) => Try(c +: s)
    )
  }

  def digit: StringSerializer[Char] = item.satisfy(_.isDigit, a => s"input '$a' is not a digit")

  def number: StringSerializer[String] = digit.atLeastOnce.contramap(_.toList)

  def lower: StringSerializer[Char] = item.satisfy(_.isLower, a => s"input '$a' is not a lowercase character")

  def upper: StringSerializer[Char] = item.satisfy(_.isUpper, a => s"input '$a' is not an uppercase character")

  def letter: StringSerializer[Char] = item.satisfy(_.isLetter, a => s"input '$a' is not a letter")

  def alphanum: StringSerializer[Char] = item.satisfy(c => c.isLetter || c.isDigit, a => s"input '$a' is not an alphanumeric character")

  def char(c: Char): StringSerializer[Char] = item.satisfy(c ==, a => s"input '$a' is not equal to '$c'")

  def space: StringSerializer[Char] = char(' ')

  def string(s: String): StringSerializer[String] = {
    s.toList match {
      case x :: xs => char(x).contramap[String](_.head).combine(string(xs.mkString).contramap(_.tail))
      case Nil => from("")
    }
  }
}
