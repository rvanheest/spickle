package com.github.rvanheest.spickle.parser

import com.github.rvanheest.spickle.parser.xml.XmlParser.XmlParser

import scala.util.{ Failure, Success, Try }

class Parser[S, A](parse: S => (Try[A], S)) {

	def run(s: S): (Try[A], S) = parse(s)

	def eval(s: S): Try[A] = parse(s)._1

	def execute(s: S): S = parse(s)._2

	def orElse[B >: A](other: => Parser[S, B]): Parser[S, B] = {
		Parser(st => {
			parse(st) match {
				case res@(Success(_), _) => res
				case (Failure(_), _) => other.run(st)
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

	def flatMap[B](f: A => Parser[S, B]): Parser[S, B] = {
		Parser(st => {
			parse(st) match {
				case (Success(a), st2) => f(a).run(st2)
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
		this >>= (x => if (predicate(x)) Parser.from(x) else Parser.empty)
	}
	def filter(predicate: A => Boolean): Parser[S, A] = this.satisfy(predicate)

  // TODO improve error message
	def noneOf(as: List[A]): Parser[S, A] = this.satisfy(!as.contains(_))

	def maybe: Parser[S, Option[A]] = this.map(Option(_)) <|> Parser.from(Option.empty)

	def many: Parser[S, List[A]] = this.atLeastOnce <|> Parser.from(Nil)

	def atLeastOnce: Parser[S, List[A]] = {
		for {
			x <- this
			xs <- this.many
		} yield x :: xs
	}

	def takeUntil(predicate: A => Boolean): Parser[S, List[A]] = this.takeWhile(!predicate(_))

	def takeWhile(predicate: A => Boolean): Parser[S, List[A]] = this.satisfy(predicate).many

	def separatedBy[Sep](sep: Parser[S, Sep]): Parser[S, List[A]] = {
    this.separatedBy1(sep) <|> Parser.from(Nil)
	}

	def separatedBy1[Sep](sep: Parser[S, Sep]): Parser[S, List[A]] = {
		for {
			x <- this
			xs <- (sep >> this).many
		} yield x :: xs
	}

	def skipMany: Parser[S, Unit] = this >> this.skipMany <|> Parser.from(())
}

object Parser {
	def apply[S, A](parser: S => (Try[A], S)) = new Parser(parser)

	def from[S, A](a: A): Parser[S, A] = Parser((Success(a), _))

	def empty[S, A]: Parser[S, A] = Parser((Failure(new NoSuchElementException("empty parser")), _))

	def failure[S, A](e: Throwable): Parser[S, A] = Parser((Failure(e), _))

	def withException[T, S, A](s: S)(constructor: S => A): Parser[T, A] = {
		try { Parser.from(constructor(s)) }
		catch { case e: Throwable => Parser.failure(e) }
	}

	def debugAndFail(pos: String = ""): XmlParser[Nothing] = {
		Parser(xs => sys.error(s"you hit a debug statement at $pos: $xs"))
	}

	def debugAndContinue(pos: String = ""): XmlParser[Unit] = {
		Parser(xs => {
			println(s"you hit a debug statement at $pos: $xs")
			(Success(()), xs)
		})
	}
}
