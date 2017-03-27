package com.github.rvanheest.spickle.pickle

import com.github.rvanheest.spickle.parser.Parser

import scala.language.higherKinds
import scala.reflect.{ ClassTag, classTag }
import scala.util.{ Failure, Try }

// TODO improve error messages
class Pickle[A, State](val pickle: (A, State) => Try[State],
																val unpickle: State => (Try[A], State)) {

	private[pickle] def parse: Parser[State, A] = Parser(this.unpickle)

	def seqId: SeqBuilder[A, A, State] = new SeqBuilder(this, identity)

	def seq[B](f: B => A): SeqBuilder[A, B, State] = new SeqBuilder(this, f)

	def upcast[B >: A](implicit ctA: ClassTag[A], ctB: ClassTag[B]): Pickle[B, State] = {
		this.seq[B] {
			case a: A => a
			case _ => sys.error(s"can't cast ${ classTag[B] } to ${ classTag[A] }")
		}.map(identity)
	}

	def orElse(other: => Pickle[A, State]): Pickle[A, State] = {
		Pickle(
			pickle = (a, state) => this.pickle(a, state) orElse other.pickle(a, state),
			unpickle = (this.parse <|> other.parse).parse)
	}

	def satisfy(predicate: A => Boolean): Pickle[A, State] = {
		Pickle(
			pickle = (a, state) => if (predicate(a)) this.pickle(a, state)
			else Failure(new NoSuchElementException("empty pickle")),
			unpickle = this.parse.satisfy(predicate).parse)
	}

	def noneOf(as: Seq[A]): Pickle[A, State] = satisfy(!as.contains(_))

	def maybe: Pickle[Option[A], State] = {
		Pickle(
			pickle = (optA, state) => optA.map(this.pickle(_, state)).getOrElse(Try(state)),
			unpickle = this.parse.maybe.parse)
	}

	def many: Pickle[Seq[A], State] = {
		Pickle(
			pickle = (as, state) => as.foldRight(Try(state))((a, triedState) => triedState.flatMap(this.pickle(a, _))),
			unpickle = this.parse.many.parse)
	}

	def atLeastOnce: Pickle[Seq[A], State] = {
		for {
			x <- this.seq[Seq[A]](_.head)
			xs <- many.seq[Seq[A]](_.tail)
		} yield x +: xs
	}

	def takeUntil(predicate: A => Boolean): Pickle[Seq[A], State] = takeWhile(!predicate(_))

	def takeWhile(predicate: A => Boolean): Pickle[Seq[A], State] = {
		Pickle(
			pickle = this.satisfy(predicate).many.pickle,
			unpickle = this.parse.takeWhile(predicate).parse)
	}

	def separatedBy[Sep](separator: Sep)(sep: Pickle[Sep, State]): Pickle[Seq[A], State] = {
		Pickle(
			pickle = (as, state) => this.separatedBy1(separator)(sep).pickle(as, state) orElse Try(state),
			unpickle = this.parse.separatedBy(sep.parse).parse)
	}

	def separatedBy1[Sep](separator: Sep)(sep: Pickle[Sep, State]): Pickle[Seq[A], State] = {
		for {
			x <- this.seq[Seq[A]](_.head)
			xs <- sep.seq[A](_ => separator).flatMap(_ => this).many.seq[Seq[A]](_.tail)
		} yield x +: xs
	}
}

object Pickle {
	def apply[A, State](pickle: (A, State) => Try[State],
											unpickle: State => (Try[A], State)): Pickle[A, State] = {
		new Pickle(pickle, unpickle)
	}

	implicit class StringOperators[State](val pickle: Pickle[String, State]) extends AnyVal {
		def toByte: Pickle[Byte, State] = {
			pickle.seq[Byte](_.toString).map(_.toByte)
		}

		def toShort: Pickle[Short, State] = {
			pickle.seq[Short](_.toString).map(_.toShort)
		}

		def toInt: Pickle[Int, State] = {
			pickle.seq[Int](_.toString).map(_.toInt)
		}

		def toLong: Pickle[Long, State] = {
			pickle.seq[Long](_.toString).map(_.toLong)
		}

		def toFloat: Pickle[Float, State] = {
			pickle.seq[Float](_.toString).map(_.toFloat)
		}

		def toDouble: Pickle[Double, State] = {
			pickle.seq[Double](_.toString).map(_.toDouble)
		}
	}
}
