package com.github.rvanheest.spickle.parser

import scala.util.{ Failure, Success, Try }

class Parser[S, A](val parse: S => (Try[A], S)) {

  def eval(s: S): Try[A] = parse(s)._1

  def execute(s: S): S = parse(s)._2

  def orElse[B >: A](other: => Parser[S, B]): Parser[S, B] = {
    Parser(st => {
      parse(st) match {
        case res @ (Success(_), _) => res
        case (Failure(_), _) => other.parse(st)
      }
    })
  }

  def <|>[B >: A](other: => Parser[S, B]): Parser[S, B] = this.orElse(other)

  def map[B](f: A => B): Parser[S, B] = {
    Parser(st => {
      parse(st) match {
        case (Success(a), st2) => (Success(f(a)), st2)
        case (Failure(e), st2) => (Failure(e), st2)
      }
    })
  }

  def as[B](b: => B): Parser[S, B] = this.map(_ => b)

  def void: Parser[S, Unit] = this.as(())

  def flatMap[B](f: A => Parser[S, B]): Parser[S, B] = {
    Parser(st => {
      parse(st) match {
        case (Success(a), st2) => f(a).parse(st2)
        case (Failure(e), st2) => (Failure(e), st2)
      }
    })
  }

  def >>=[B](f: A => Parser[S, B]): Parser[S, B] = this.flatMap(f)

  def transform[B](f: (A, S) => (Try[B], S)): Parser[S, B] = {
    Parser(st => {
      parse(st) match {
        case (Success(a), st2) => f(a, st2)
        case (Failure(e), st2) => (Failure(e), st2)
      }
    })
  }

  def >>[B](other: => Parser[S, B]): Parser[S, B] = this >>= (_ => other)

  def <<[B](other: => Parser[S, B]): Parser[S, A] = this >>= (x => other >> Parser.from(x))

  def satisfy(predicate: A => Boolean): Parser[S, A] = {
    this.satisfy(predicate, a => s"input '$a' did not satisfy predicate")
  }

  def satisfy(predicate: A => Boolean, errMsg: A => String): Parser[S, A] = {
    this >>= (x => if (predicate(x)) Parser.from(x)
                   else Parser.failure(ParserFailedException(errMsg(x))))
  }

  def filter(predicate: A => Boolean): Parser[S, A] = this.satisfy(predicate)

  def noneOf(as: Seq[A]): Parser[S, A] = {
    this.satisfy(!as.contains(_), a => s"input '$a' did contain any of ${ as.mkString("[", ", ", "]") }")
  }

  def maybe: Parser[S, Option[A]] = this.map(Option(_)) <|> Parser.from(Option.empty)

  def many: Parser[S, Seq[A]] = this.atLeastOnce <|> Parser.from(Nil)

  def atLeastOnce: Parser[S, Seq[A]] = {
    for {
      x <- this
      xs <- this.many
    } yield x +: xs
  }

  def takeUntil(predicate: A => Boolean): Parser[S, Seq[A]] = this.takeWhile(!predicate(_))

  def takeWhile(predicate: A => Boolean): Parser[S, Seq[A]] = this.satisfy(predicate).many

  def separatedBy[Sep](sep: Parser[S, Sep]): Parser[S, Seq[A]] = {
    this.separatedBy1(sep) <|> Parser.from(Nil)
  }

  def separatedBy1[Sep](sep: Parser[S, Sep]): Parser[S, Seq[A]] = {
    for {
      x <- this
      xs <- (sep >> this).many
    } yield x +: xs
  }

  def skipMany: Parser[S, Unit] = this >> this.skipMany <|> Parser.from(())
}

object Parser {
  def apply[S, A](parser: S => (Try[A], S)) = new Parser(parser)

  def from[S, A](a: A): Parser[S, A] = Parser((Success(a), _))

  def empty[S, A]: Parser[S, A] = Parser((Failure(new NoSuchElementException("empty parser")), _))

  def failure[S, A](e: Throwable): Parser[S, A] = Parser((Failure(e), _))

  def debugAndFail[S](pos: String = ""): Parser[S, Nothing] = {
    Parser(xs => sys.error(s"you hit a debug statement at $pos: $xs"))
  }

  def debugAndContinue[S](pos: String = ""): Parser[S, Unit] = {
    Parser(xs => {
      println(s"you hit a debug statement at $pos: $xs")
      (Success(()), xs)
    })
  }

  implicit class StringOperators[State](val parser: Parser[State, String]) extends AnyVal {
    def toByte: Parser[State, Byte] = parser.map(_.toByte)

    def toShort: Parser[State, Short] = parser.map(_.toShort)

    def toInt: Parser[State, Int] = parser.map(_.toInt)

    def toLong: Parser[State, Long] = parser.map(_.toLong)

    def toFloat: Parser[State, Float] = parser.map(_.toFloat)

    def toDouble: Parser[State, Double] = parser.map(_.toDouble)
  }
}
