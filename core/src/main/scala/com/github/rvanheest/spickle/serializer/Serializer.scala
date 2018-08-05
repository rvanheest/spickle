package com.github.rvanheest.spickle.serializer

import scala.reflect.{ ClassTag, classTag }
import scala.util.{ Failure, Try }

class Serializer[State, A](private[serializer] val serializer: (A, State) => Try[State]) {

  def serialize(a: A, state: State): Try[State] = serializer(a, state)

  def contramap[B](f: B => A): Serializer[State, B] = {
    Serializer((b, state) => for {
      a <- Try { f(b) }
      state2 <- this.serializer(a, state)
    } yield state2)
  }

  private[spickle] def contramapCombine[B](f: B => A, g: A => Serializer[State, B]): Serializer[State, B] = {
    Serializer((b, state) => for {
      a <- Try { f(b) }
      state2 <- g(a).serialize(b, state)
      state3 <- this.serialize(a, state2)
    } yield state3)
  }

  def combine(that: Serializer[State, A]): Serializer[State, A] = {
    Serializer((a, state) => {
      for {
        state2 <- that.serializer(a, state)
        state3 <- this.serializer(a, state2)
      } yield state3
    })
  }

  def upcast[B >: A](implicit ctA: ClassTag[A]): Serializer[State, B] = {
    this.contramap {
      case a: A => a
      case x => throw SerializerFailedException(s"can't cast ${ x.getClass } to ${ classTag[A] }")
    }
  }

  def orElse(other: => Serializer[State, A]): Serializer[State, A] = {
    Serializer((a, state) => this.serializer(a, state) orElse other.serializer(a, state))
  }

  def satisfy(predicate: A => Boolean): Serializer[State, A] = {
    this.satisfy(predicate, a => s"input '$a' did not satisfy predicate")
  }

  def satisfy(predicate: A => Boolean, errMsg: A => String): Serializer[State, A] = {
    Serializer((a, state) => if (predicate(a)) this.serializer(a, state)
                             else Failure(SerializerFailedException(errMsg(a))))
  }

  def noneOf(as: Seq[A]): Serializer[State, A] = {
    this.satisfy(!as.contains(_), a => s"input '$a' is contained in ${ as.mkString("[", ", ", "]") }")
  }

  def maybe: Serializer[State, Option[A]] = {
    Serializer((optA, state) => optA.map(this.serializer(_, state)).getOrElse(Try(state)))
  }

  def many: Serializer[State, Seq[A]] = {
    Serializer((as, state) => as.foldRight(Try(state))((a, triedState) => triedState.flatMap(this.serializer(a, _))))
  }

  def atLeastOnce: Serializer[State, Seq[A]] = {
    val headSerializer = this.contramap[Seq[A]](_.head)
    val tailSerializer = many.contramap[Seq[A]](_.tail)
    headSerializer.combine(tailSerializer)
  }

  def takeUntil(predicate: A => Boolean): Serializer[State, Seq[A]] = takeWhile(!predicate(_))

  def takeWhile(predicate: A => Boolean): Serializer[State, Seq[A]] = {
    this.satisfy(predicate).many
  }

  def separatedBy[Sep](separator: Sep)(sep: Serializer[State, Sep]): Serializer[State, Seq[A]] = {
    Serializer((as, state) => this.separatedBy1(separator)(sep).serializer(as, state) orElse Try(state))
  }

  def separatedBy1[Sep](separator: Sep)(sep: Serializer[State, Sep]): Serializer[State, Seq[A]] = {
    val headSerializer = this.contramap[Seq[A]](_.head)
    val tailSerializer = sep.contramap[A](_ => separator)
      .combine(this)
      .many
      .contramap[Seq[A]](_.tail)

    headSerializer.combine(tailSerializer)
  }
}

object Serializer {
  def apply[State, A](serializer: (A, State) => Try[State]): Serializer[State, A] = {
    new Serializer(serializer)
  }

  def from[State, A](a: A): Serializer[State, A] = {
    Serializer(
      (_, state) => Try { state }
    )
  }

  def empty[State, A]: Serializer[State, A] = {
    Serializer(
      (_, _) => Failure(new NoSuchElementException("empty serializer"))
    )
  }

  def failure[State, A](e: Throwable): Serializer[State, A] = {
    Serializer(
      (_, _) => Failure(e)
    )
  }

  def debugAndFail[State](pos: String): Serializer[State, Nothing] = {
    Serializer[State, Nothing](
      (_, state) => sys.error(s"you hit a debug statement at $pos: $state")
    )
  }

  def debugAndContinue[State](pos: String)(implicit debugger: String => Unit = println): Serializer[State, Unit] = {
    Serializer(
      (a, state) => Try {
        debugger(s"you hit a debug statement at $pos while serializing $a with state $state")
        state
      }
    )
  }

  implicit class StringOperators[State](val serializer: Serializer[State, String]) extends AnyVal {

    def fromByte: Serializer[State, Byte] = serializer.contramap(_.toString)

    def fromShort: Serializer[State, Short] = serializer.contramap(_.toString)

    def fromInt: Serializer[State, Int] = serializer.contramap(_.toString)

    def fromLong: Serializer[State, Long] = serializer.contramap(_.toString)

    def fromFloat: Serializer[State, Float] = serializer.contramap(_.toString)

    def fromDouble: Serializer[State, Double] = serializer.contramap(_.toString)
  }

  implicit class MaybeStringOperators[State](val serializer: Serializer[State, Option[String]]) extends AnyVal {

    def fromByte: Serializer[State, Option[Byte]] = serializer.contramap(_.map(_.toString))

    def fromShort: Serializer[State, Option[Short]] = serializer.contramap(_.map(_.toString))

    def fromInt: Serializer[State, Option[Int]] = serializer.contramap(_.map(_.toString))

    def fromLong: Serializer[State, Option[Long]] = serializer.contramap(_.map(_.toString))

    def fromFloat: Serializer[State, Option[Float]] = serializer.contramap(_.map(_.toString))

    def fromDouble: Serializer[State, Option[Double]] = serializer.contramap(_.map(_.toString))
  }
}
