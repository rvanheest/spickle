package com.github.rvanheest.spickle.pickle

import com.github.rvanheest.spickle.parser.Parser

import scala.language.higherKinds
import scala.reflect.{ ClassTag, classTag }
import scala.util.{ Failure, Try }

// TODO improve error messages
abstract class Pickle[A, State](val pickle: (A, State) => Try[State],
																val unpickle: State => (Try[A], State)) {

	type Repr[X] <: Pickle[X, State]

	protected[this] implicit def builder[X]: PickleBuilder[X, State, Repr[X]]
	private[pickle] def parse: Parser[State, A] = Parser(this.unpickle)

	def seq: SeqBuilder[A, A, State, Repr] = new SeqBuilder(this, identity)

	def seq[B](f: B => A): SeqBuilder[A, B, State, Repr] = new SeqBuilder(this, f)

	def upcast[B >: A](implicit ctA: ClassTag[A], ctB: ClassTag[B]): Repr[B] = {
		this.seq[B] {
			case a: A => a
			case _ => sys.error(s"can't cast ${ classTag[B] } to ${ classTag[A] }")
		}.map(identity)
	}

	def orElse(other: => Pickle[A, State]): Repr[A] = {
		builder[A](
			pickle = (a, state) => this.pickle(a, state) orElse other.pickle(a, state),
			unpickle = (this.parse <|> other.parse).run)
	}

	def satisfy(predicate: A => Boolean): Repr[A] = {
		builder[A](
			pickle = (a, state) => if (predicate(a)) this.pickle(a, state)
			else Failure(new NoSuchElementException("empty pickle")),
			unpickle = this.parse.satisfy(predicate).run)
	}

	def noneOf(as: Seq[A]): Repr[A] = satisfy(!as.contains(_))

	def maybe: Repr[Option[A]] = {
		builder[Option[A]](
			pickle = (optA, state) => optA.map(this.pickle(_, state)).getOrElse(Try(state)),
			unpickle = this.parse.maybe.run)
	}

	def many: Repr[Seq[A]] = {
		builder[Seq[A]](
			pickle = (as, state) => as.foldRight(Try(state))((a, triedState) => triedState.flatMap(this.pickle(a, _))),
			unpickle = this.parse.many.run)
	}

	def atLeastOnce: Repr[Seq[A]] = {
		for {
			x <- this.seq[Seq[A]](_.head)
			xs <- many.seq[Seq[A]](_.tail)
		} yield x +: xs
	}

	def takeUntil(predicate: A => Boolean): Repr[Seq[A]] = takeWhile(!predicate(_))

	def takeWhile(predicate: A => Boolean): Repr[Seq[A]] = {
		builder[Seq[A]](
			pickle = this.satisfy(predicate).many.pickle,
			unpickle = this.parse.takeWhile(predicate).run)
	}

	def separatedBy[Sep](separator: Sep)(sep: Repr[Sep]): Repr[Seq[A]] = {
		builder[Seq[A]](
			pickle = (as, state) => this.separatedBy1(separator)(sep).pickle(as, state) orElse Try(state),
			unpickle = this.parse.separatedBy(sep.parse).run)
	}

	def separatedBy1[Sep](separator: Sep)(sep: Repr[Sep]): Repr[Seq[A]] = {
		for {
			x <- this.seq[Seq[A]](_.head)
			xs <- sep.seq[A](_ => separator).flatMap(_ => this).many.seq[Seq[A]](_.tail)
		} yield x +: xs
	}
}


