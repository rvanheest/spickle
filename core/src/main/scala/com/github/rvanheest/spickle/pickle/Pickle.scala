package com.github.rvanheest.spickle.pickle

import com.github.rvanheest.spickle.parser.Parser

import scala.language.higherKinds
import scala.reflect.{ ClassTag, classTag }
import scala.util.{ Failure, Try }

// TODO improve error messages
class Pickle[A, State](private[pickle] val pickler: (A, State) => Try[State],
                       private[pickle] val parser: Parser[State, A]) {

  def parse(state: State): (Try[A], State) = parser.parse(state)

  def pickle(a: A, state: State): Try[State] = pickler(a, state)

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
      pickler = (a, state) => this.pickler(a, state) orElse other.pickler(a, state),
      parser = this.parser <|> other.parser)
  }

  def satisfy(predicate: A => Boolean): Pickle[A, State] = {
    Pickle(
      pickler = (a, state) => if (predicate(a)) this.pickler(a, state)
                              else Failure(new NoSuchElementException("empty pickle")),
      parser = this.parser.satisfy(predicate))
  }

  def noneOf(as: Seq[A]): Pickle[A, State] = satisfy(!as.contains(_))

  def maybe: Pickle[Option[A], State] = {
    Pickle(
      pickler = (optA, state) => optA.map(this.pickler(_, state)).getOrElse(Try(state)),
      parser = this.parser.maybe)
  }

  def many: Pickle[Seq[A], State] = {
    Pickle(
      pickler = (as, state) => as.foldRight(Try(state))((a, triedState) => triedState.flatMap(this.pickler(a, _))),
      parser = this.parser.many)
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
      pickler = this.satisfy(predicate).many.pickler,
      parser = this.parser.takeWhile(predicate))
  }

  def separatedBy[Sep](separator: Sep)(sep: Pickle[Sep, State]): Pickle[Seq[A], State] = {
    Pickle(
      pickler = (as, state) => this.separatedBy1(separator)(sep).pickler(as, state) orElse Try(state),
      parser = this.parser.separatedBy(sep.parser))
  }

  def separatedBy1[Sep](separator: Sep)(sep: Pickle[Sep, State]): Pickle[Seq[A], State] = {
    for {
      x <- this.seq[Seq[A]](_.head)
      xs <- sep.seq[A](_ => separator).flatMap(_ => this).many.seq[Seq[A]](_.tail)
    } yield x +: xs
  }
}

object Pickle {
  def apply[A, State](pickler: (A, State) => Try[State],
                      parser: Parser[State, A]): Pickle[A, State] = {
    new Pickle(pickler, parser)
  }

  implicit class StringOperators[State](val pickle: Pickle[String, State]) extends AnyVal {
    def toByte: Pickle[Byte, State] = pickle.seq[Byte](_.toString).map(_.toByte)

    def toShort: Pickle[Short, State] = pickle.seq[Short](_.toString).map(_.toShort)

    def toInt: Pickle[Int, State] = pickle.seq[Int](_.toString).map(_.toInt)

    def toLong: Pickle[Long, State] = pickle.seq[Long](_.toString).map(_.toLong)

    def toFloat: Pickle[Float, State] = pickle.seq[Float](_.toString).map(_.toFloat)

    def toDouble: Pickle[Double, State] = pickle.seq[Double](_.toString).map(_.toDouble)
  }
}
