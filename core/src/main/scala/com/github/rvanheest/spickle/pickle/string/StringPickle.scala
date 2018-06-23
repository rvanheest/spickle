package com.github.rvanheest.spickle.pickle.string

import com.github.rvanheest.spickle.parser.string.StringParser
import com.github.rvanheest.spickle.pickle.Pickle
import com.github.rvanheest.spickle.pickle.Pickle.from
import com.github.rvanheest.spickle.serializer.Serializer

import scala.language.postfixOps
import scala.util.Try

object StringPickle {

  type StringPickle[A] = Pickle[String, A]

  def item: StringPickle[Char] = {
    Pickle(
      serializer = Serializer((c, s) => Try(c +: s)),
      parser = StringParser.item)
  }

  def digit: StringPickle[Char] = item.satisfy(_.isDigit, a => s"input '$a' is not a digit")

  def number: StringPickle[String] = digit.atLeastOnce.seq[String](_.toList).map(_.mkString)

  def lower: StringPickle[Char] = item.satisfy(_.isLower, a => s"input '$a' is not a lowercase character")

  def upper: StringPickle[Char] = item.satisfy(_.isUpper, a => s"input '$a' is not an uppercase character")

  def letter: StringPickle[Char] = item.satisfy(_.isLetter, a => s"input '$a' is not a letter")

  def alphanum: StringPickle[Char] = item.satisfy(c => c.isLetter || c.isDigit, a => s"input '$a' is not an alphanumeric character")

  def char(c: Char): StringPickle[Char] = item.satisfy(c ==, a => s"input '$a' is not equal to '$c'")

  def space: StringPickle[Char] = char(' ')

  def string(s: String): StringPickle[String] = {
    s.toList match {
      case x :: xs => for {
        _ <- char(x).seq[String](_.head)
        _ <- string(xs.mkString).seq[String](_.tail)
      } yield s
      case Nil => from("")
    }
  }
}
