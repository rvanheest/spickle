package com.github.rvanheest.spickle.pickle

import com.github.rvanheest.spickle.parser.{ Parser, ParserFailedException }

import scala.language.higherKinds
import scala.reflect.{ ClassTag, classTag }
import scala.util.{ Failure, Try }

// TODO improve error messages
class Pickle[State, A](private[pickle] val pickler: (A, State) => Try[State],
                       private[pickle] val parser: Parser[State, A]) {

  def pickle(a: A, state: State): Try[State] = pickler(a, state)

  def parse(state: State): (Try[A], State) = parser.parse(state)

  def seq[B](f: B => A): SeqBuilder[State, A, B] = new SeqBuilder(this, f)

  def seqId: SeqBuilder[State, A, A] = new SeqBuilder(this, identity)

  def upcast[B >: A](implicit ctA: ClassTag[A], ctB: ClassTag[B]): Pickle[State, B] = {
    this.seq[B] {
      case a: A => a
      case x => throw PickleFailedException(s"can't cast ${ x.getClass } to ${ classTag[A] }")
    }.map(identity)
  }

  def orElse(other: => Pickle[State, A]): Pickle[State, A] = {
    Pickle(
      pickler = (a, state) => this.pickler(a, state) orElse other.pickler(a, state),
      parser = this.parser <|> other.parser)
  }

  def satisfy(predicate: A => Boolean): Pickle[State, A] = {
    this.satisfy(predicate, a => s"input '$a' did not satisfy predicate")
  }

  def satisfy(predicate: A => Boolean, errMsg: A => String): Pickle[State, A] = {
    Pickle(
      pickler = (a, state) => if (predicate(a)) this.pickler(a, state)
                              else Failure(PickleFailedException(errMsg(a))),
      parser = this.parser.satisfy(predicate, errMsg))
  }

  def noneOf(as: Seq[A]): Pickle[State, A] = {
    this.satisfy(!as.contains(_), a => s"input '$a' is contained in ${ as.mkString("[", ", ", "]") }")
  }

  def maybe: Pickle[State, Option[A]] = {
    Pickle(
      pickler = (optA, state) => optA.map(this.pickler(_, state)).getOrElse(Try(state)),
      parser = this.parser.maybe)
  }

  def many: Pickle[State, Seq[A]] = {
    Pickle(
      pickler = (as, state) => as.foldRight(Try(state))((a, triedState) => triedState.flatMap(this.pickler(a, _))),
      parser = this.parser.many)
  }

  def atLeastOnce: Pickle[State, Seq[A]] = {
    for {
      x <- this.seq[Seq[A]](_.head)
      xs <- many.seq[Seq[A]](_.tail)
    } yield x +: xs
  }

  def takeUntil(predicate: A => Boolean): Pickle[State, Seq[A]] = takeWhile(!predicate(_))

  def takeWhile(predicate: A => Boolean): Pickle[State, Seq[A]] = {
    Pickle(
      pickler = this.satisfy(predicate).many.pickler,
      parser = this.parser.takeWhile(predicate))
  }

  def separatedBy[Sep](separator: Sep)(sep: Pickle[State, Sep]): Pickle[State, Seq[A]] = {
    Pickle(
      pickler = (as, state) => this.separatedBy1(separator)(sep).pickler(as, state) orElse Try(state),
      parser = this.parser.separatedBy(sep.parser))
  }

  def separatedBy1[Sep](separator: Sep)(sep: Pickle[State, Sep]): Pickle[State, Seq[A]] = {
    for {
      x <- this.seq[Seq[A]](_.head)
      xs <- sep.seq[A](_ => separator).flatMap(_ => this).many.seq[Seq[A]](_.tail)
    } yield x +: xs
  }
}

object Pickle {
  def apply[State, A](pickler: (A, State) => Try[State],
                      parser: Parser[State, A]): Pickle[State, A] = {
    new Pickle(pickler, parser)
  }

  def from[State, A](a: A): Pickle[State, A] = Pickle(
    pickler = (_, state) => Try { state },
    parser = Parser.from(a))

  def empty[State, A]: Pickle[State, A] = Pickle(
    pickler = (_, _) => Failure(new NoSuchElementException("empty parser")),
    parser = Parser.empty)

  def failure[State, A](e: Throwable): Pickle[State, A] = Pickle(
    pickler = (_, _) => Failure(e),
    parser = Parser.failure(e))

  implicit class StringOperators[State](val pickle: Pickle[State, String]) extends AnyVal {
    def toByte: Pickle[State, Byte] = pickle.seq[Byte](_.toString).map(_.toByte)

    def toShort: Pickle[State, Short] = pickle.seq[Short](_.toString).map(_.toShort)

    def toInt: Pickle[State, Int] = pickle.seq[Int](_.toString).map(_.toInt)

    def toLong: Pickle[State, Long] = pickle.seq[Long](_.toString).map(_.toLong)

    def toFloat: Pickle[State, Float] = pickle.seq[Float](_.toString).map(_.toFloat)

    def toDouble: Pickle[State, Double] = pickle.seq[Double](_.toString).map(_.toDouble)
  }
}
