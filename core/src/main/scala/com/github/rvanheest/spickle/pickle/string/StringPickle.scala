package com.github.rvanheest.spickle.pickle.string

import com.github.rvanheest.spickle.parser.string.StringParser
import com.github.rvanheest.spickle.pickle.Pickle

import scala.language.postfixOps
import scala.util.Try

object StringPickle {

  type StringPickle[A] = Pickle[String, A]

  def item: StringPickle[Char] = {
    Pickle(
      pickler = (c, s) => Try(c +: s),
      parser = StringParser.item)
  }

  def digit: StringPickle[Char] = item.satisfy(_.isDigit)

  def number: StringPickle[String] = digit.atLeastOnce.seq[String](_.toList).map(_.mkString)

  def lower: StringPickle[Char] = item.satisfy(_.isLower)

  def upper: StringPickle[Char] = item.satisfy(_.isUpper)

  def letter: StringPickle[Char] = item.satisfy(_.isLetter)

  def alphanum: StringPickle[Char] = item.satisfy(c => c.isLetter || c.isDigit)

  def char(c: Char): StringPickle[Char] = item.satisfy(c ==)

  def space: StringPickle[Char] = char(' ')

  def string(s: String): StringPickle[String] = {
    Pickle(
      pickler = (str, state) =>
        s.toList match {
          case x :: xs => (for {
            _ <- char(x).seq[String](_.head)
            _ <- string(xs.mkString).seq[String](_.tail)
          } yield s).pickler(str, state)
          case Nil => Try(state)
        },
      parser = StringParser.string(s))
  }
}
