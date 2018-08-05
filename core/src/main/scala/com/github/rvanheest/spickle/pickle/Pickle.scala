package com.github.rvanheest.spickle.pickle

import com.github.rvanheest.spickle.parser.Parser
import com.github.rvanheest.spickle.serializer.Serializer

import scala.reflect.ClassTag
import scala.util.Try

class Pickle[State, A](private[pickle] val serializer: Serializer[State, A],
                       private[pickle] val parser: Parser[State, A]) {

  def serialize(a: A, state: State): Try[State] = serializer.serialize(a, state)

  def parse(state: State): (Try[A], State) = parser.parse(state)

  def eval(input: State): Try[A] = parser.eval(input)

  def execute(input: State): State = parser.execute(input)

  def seq[B](f: B => A): SeqBuilder[State, A, B] = new SeqBuilder(this, f)

  def seqId: SeqBuilder[State, A, A] = new SeqBuilder(this, identity)

  def upcast[B >: A](implicit ctA: ClassTag[A]): Pickle[State, B] = {
    Pickle(
      serializer = serializer.upcast[B],
      parser = parser.map(identity)
    )
  }

  def orElse(other: => Pickle[State, A]): Pickle[State, A] = {
    Pickle(
      serializer = this.serializer orElse other.serializer,
      parser = this.parser orElse other.parser
    )
  }

  def satisfy(predicate: A => Boolean): Pickle[State, A] = {
    this.satisfy(predicate, a => s"input '$a' did not satisfy predicate")
  }

  def satisfy(predicate: A => Boolean, errMsg: A => String): Pickle[State, A] = {
    Pickle(
      serializer = this.serializer.satisfy(predicate, errMsg),
      parser = this.parser.satisfy(predicate, errMsg)
    )
  }

  def noneOf(as: Seq[A]): Pickle[State, A] = {
    this.satisfy(!as.contains(_), a => s"input '$a' is contained in ${ as.mkString("[", ", ", "]") }")
  }

  def maybe: Pickle[State, Option[A]] = {
    Pickle(
      serializer = this.serializer.maybe,
      parser = this.parser.maybe
    )
  }

  def many: Pickle[State, Seq[A]] = {
    Pickle(
      serializer = this.serializer.many,
      parser = this.parser.many
    )
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
      serializer = this.serializer.takeWhile(predicate),
      parser = this.parser.takeWhile(predicate)
    )
  }

  def separatedBy[Sep](separator: Sep)(sep: Pickle[State, Sep]): Pickle[State, Seq[A]] = {
    Pickle(
      serializer = this.serializer.separatedBy(separator)(sep.serializer),
      parser = this.parser.separatedBy(sep.parser)
    )
  }

  def separatedBy1[Sep](separator: Sep)(sep: Pickle[State, Sep]): Pickle[State, Seq[A]] = {
    for {
      x <- this.seq[Seq[A]](_.head)
      xs <- sep.seq[A](_ => separator).flatMap(_ => this).many.seq[Seq[A]](_.tail)
    } yield x +: xs
  }
}

object Pickle {
  def apply[State, A](serializer: Serializer[State, A],
                      parser: Parser[State, A]): Pickle[State, A] = {
    new Pickle(serializer, parser)
  }

  def from[State, A](a: A): Pickle[State, A] = {
    Pickle(
      serializer = Serializer.from[State, A](a),
      parser = Parser.from(a)
    )
  }

  def empty[State, A]: Pickle[State, A] = {
    Pickle(
      serializer = Serializer.empty,
      parser = Parser.empty
    )
  }

  def failure[State, A](e: Throwable): Pickle[State, A] = {
    Pickle(
      serializer = Serializer.failure(e),
      parser = Parser.failure(e)
    )
  }

  def debugAndFail[State](pos: String): Pickle[State, Nothing] = {
    Pickle[State, Nothing](
      serializer = Serializer.debugAndFail(pos),
      parser = Parser.debugAndFail(pos)
    )
  }

  def debugAndContinue[State](pos: String)(implicit debugger: String => Unit = println): Pickle[State, Unit] = {
    Pickle(
      serializer = Serializer.debugAndContinue(pos),
      parser = Parser.debugAndContinue(pos)
    )
  }

  implicit class StringOperators[State](val pickle: Pickle[State, String]) extends AnyVal {
    def toByte: Pickle[State, Byte] = pickle.seq[Byte](_.toString).map(_.toByte)

    def toShort: Pickle[State, Short] = pickle.seq[Short](_.toString).map(_.toShort)

    def toInt: Pickle[State, Int] = pickle.seq[Int](_.toString).map(_.toInt)

    def toLong: Pickle[State, Long] = pickle.seq[Long](_.toString).map(_.toLong)

    def toFloat: Pickle[State, Float] = pickle.seq[Float](_.toString).map(_.toFloat)

    def toDouble: Pickle[State, Double] = pickle.seq[Double](_.toString).map(_.toDouble)
  }

  implicit class MaybeStringOperators[State](val pickle: Pickle[State, Option[String]]) extends AnyVal {
    def toByte: Pickle[State, Option[Byte]] = pickle.seq[Option[Byte]](_.map(_.toString)).map(_.map(_.toByte))

    def toShort: Pickle[State, Option[Short]] = pickle.seq[Option[Short]](_.map(_.toString)).map(_.map(_.toShort))

    def toInt: Pickle[State, Option[Int]] = pickle.seq[Option[Int]](_.map(_.toString)).map(_.map(_.toInt))

    def toLong: Pickle[State, Option[Long]] = pickle.seq[Option[Long]](_.map(_.toString)).map(_.map(_.toLong))

    def toFloat: Pickle[State, Option[Float]] = pickle.seq[Option[Float]](_.map(_.toString)).map(_.map(_.toFloat))

    def toDouble: Pickle[State, Option[Double]] = pickle.seq[Option[Double]](_.map(_.toString)).map(_.map(_.toDouble))
  }
}
