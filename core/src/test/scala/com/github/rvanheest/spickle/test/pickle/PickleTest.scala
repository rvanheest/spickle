package com.github.rvanheest.spickle.test.pickle

import java.util.concurrent.atomic.AtomicInteger

import com.github.rvanheest.spickle.parser.{ Parser, ParserFailedException }
import com.github.rvanheest.spickle.pickle.string.StringPickle
import com.github.rvanheest.spickle.pickle.{ Pickle, PickleFailedException }
import com.github.rvanheest.spickle.serializer.{ Serializer, SerializerFailedException }
import org.scalatest.{ FlatSpec, Matchers }

import scala.reflect.classTag
import scala.util.{ Failure, Success, Try }

class PickleTest extends FlatSpec with Matchers {

  private val emptyError = ParserFailedException("you're trying to parse a character in an empty String")

  private type TestPickle = Pickle[String, Int]

  private def point: TestPickle = pointWithSideEffect(() => ())

  private def pointWithSideEffect[U](sideEffect: () => U): TestPickle = new TestPickle(
    serializer = Serializer((n, s) => Try {
      sideEffect()
      n + s
    }),
    parser = Parser(s => {
      sideEffect()
      s.toList match {
        case Nil => (Failure(emptyError), "")
        case p :: ps => (Try(p.toString.toInt), ps.mkString)
      }
    }))

  "pickle" should "run the pickler function in the Pickler" in {
    point.serialize(1, "23") should matchPattern { case Success("123") => }
  }

  "parse" should "run the parser function in the Pickler" in {
    point.parse("123") should matchPattern { case (Success(1), "23") => }
  }

  case class IntWrapper(integer: Int)

  "seq map" should "unpack the integer and concat it to the rest of the input" in {
    point.seq[IntWrapper](_.integer)
      .map(IntWrapper)
      .serialize(IntWrapper(1), "234") should matchPattern { case Success("1234") => }
  }

  it should "pack the first character up into the IntWrapper" in {
    point.seq[IntWrapper](_.integer)
      .map(IntWrapper)
      .parse("1234") should matchPattern { case (Success(IntWrapper(1)), "234") => }
  }

  it should "not use the unpack function while parsing" in {
    point.seq[IntWrapper](w => { fail("this position should not be visited"); w.integer })
      .map(IntWrapper)
      .parse("1234")._1 shouldBe a[Success[_]]
  }

  it should "fail if the unpack function throws an exception" in {
    val err = new Exception("err")
    point.seq[IntWrapper](_ => throw err)
      .map(IntWrapper)
      .serialize(IntWrapper(1), "234") should matchPattern { case Failure(`err`) => }
  }

  it should "not execute the original Pickler if the unpack function throws an exception" in {
    pointWithSideEffect(() => fail("this position should not be visited"))
      .seq[IntWrapper](_ => throw new Exception("err"))
      .map(IntWrapper)
      .serialize(IntWrapper(1), "234") shouldBe a[Failure[_]]
  }

  it should "fail if the original Pickler fails" in {
    val err = new Exception("err")
    pointWithSideEffect(() => throw err)
      .seq[IntWrapper](_.integer)
      .map(IntWrapper)
      .serialize(IntWrapper(1), "234") should matchPattern { case Failure(`err`) => }
  }

  it should "not use the map function when pickling" in {
    point.seq[IntWrapper](_.integer)
      .map(i => { fail("this position should not be visited"); IntWrapper(i) })
      .serialize(IntWrapper(1), "234") shouldBe a[Success[_]]
  }

  case class IntTupleWrapper(left: Int, right: Int)

  "seq flatMap" should "unpack the integers and concat them to the rest of the input" in {
    point.seq[IntTupleWrapper](_.left)
      .flatMap(i1 => point.seq[IntTupleWrapper](_.right).map(IntTupleWrapper(i1, _)))
      .serialize(IntTupleWrapper(1, 2), "34") should matchPattern { case Success("1234") => }
  }

  it should "pack the first two characters up into the IntTupleWrapper" in {
    point.seq[IntTupleWrapper](_.left)
      .flatMap(i1 => point.seq[IntTupleWrapper](_.right).map(IntTupleWrapper(i1, _)))
      .parse("1234") should matchPattern { case (Success(IntTupleWrapper(1, 2)), "34") => }
  }

  it should "not use the unpack functions while parsing" in {
    point.seq[IntTupleWrapper](w => { fail("this position should not be visited"); w.left })
      .flatMap(i1 => point
        .seq[IntTupleWrapper](w => { fail("this position should not be visited"); w.right })
        .map(IntTupleWrapper(i1, _)))
      .parse("1234")._1 shouldBe a[Success[_]]
  }

  it should "fail if the unpack function throws an exception" in {
    val err = new Exception("err")
    point.seq[IntTupleWrapper](w => { throw err; w.left })
      .flatMap(i1 => point.seq[IntTupleWrapper](_.right).map(IntTupleWrapper(i1, _)))
      .serialize(IntTupleWrapper(1, 2), "34") should matchPattern { case Failure(`err`) => }
  }

  it should "not execute the flatMap function if the unpack function throws an exception" in {
    val err = new Exception("err")
    point.seq[IntTupleWrapper](w => { throw err; w.left })
      .flatMap(i1 => {
        fail("this position should not be visited")
        point.seq[IntTupleWrapper](_.right).map(IntTupleWrapper(i1, _))
      })
      .serialize(IntTupleWrapper(1, 2), "34") should matchPattern { case Failure(`err`) => }
  }

  it should "not execute the original Pickler if the unpack function throws an exception" in {
    val err = new Exception("err")
    pointWithSideEffect(() => fail("this position should not be visited"))
      .seq[IntTupleWrapper](w => { throw err; w.left })
      .flatMap(i1 => point.seq[IntTupleWrapper](_.right).map(IntTupleWrapper(i1, _)))
      .serialize(IntTupleWrapper(1, 2), "34") shouldBe a[Failure[_]]
  }

  it should "fail if the flatMap function throws an exception" in {
    val err = new Exception("err")
    point.seq[IntTupleWrapper](_.left)
      .flatMap(i1 => {
        throw err
        point.seq[IntTupleWrapper](_.right).map(IntTupleWrapper(i1, _))
      })
      .serialize(IntTupleWrapper(1, 2), "34") should matchPattern { case Failure(`err`) => }
  }

  it should "not execute the original Pickler if the flatMap function throws an exception" in {
    val err = new Exception("err")
    pointWithSideEffect(() => fail("this position should not be visited"))
      .seq[IntTupleWrapper](_.left)
      .flatMap(i1 => {
        throw err
        point.seq[IntTupleWrapper](_.right).map(IntTupleWrapper(i1, _))
      })
      .serialize(IntTupleWrapper(1, 2), "34") shouldBe a[Failure[_]]
  }

  it should "fail if the original Pickler fails" in {
    val err = new Exception("err")
    pointWithSideEffect(() => throw err).seq[IntTupleWrapper](_.left)
      .flatMap(i1 => point.seq[IntTupleWrapper](_.right)
        .map(IntTupleWrapper(i1, _)))
      .serialize(IntTupleWrapper(1, 2), "34") should matchPattern { case Failure(`err`) => }
  }

  class A()
  class B() extends A()
  class C() extends A()

  "upcast" should "cast a type to a given supertype" in {
    val pickleB: Pickle[String, B] = Pickle.from(new B())
    val pickleA: Pickle[String, A] = pickleB.upcast[A]
    pickleA.serialize(new B(), "abc") shouldBe a[Success[_]]
  }

  it should "fail in casting if the pickler has an input of a different type" in {
    val pickleB: Pickle[String, B] = Pickle.from(new B())
    val pickleA: Pickle[String, A] = pickleB.upcast[A]
    val expectedMsg = s"can't cast ${ classOf[C] } to ${ classTag[B] }"
    pickleA.serialize(new C(), "abc") should matchPattern {
      case Failure(PickleFailedException(`expectedMsg`)) =>
    }
  }

  "orElse" should "use the first pickler if this one succeeds" in {
    point.orElse(Pickle.from(1)).serialize(1, "23") should matchPattern { case Success("123") => }
  }

  it should "not use the second pickler if the first one succeeds" in {
    lazy val p2 = pointWithSideEffect(() => fail("this position should not be visited"))
    point.orElse(p2).serialize(1, "23") shouldBe a[Success[_]]
  }

  it should "use the second pickler if the first one fails" in {
    val err = new Exception("err")
    pointWithSideEffect(() => throw err)
      .orElse(Pickle.from(99))
      .serialize(1, "23") should matchPattern { case Success("23") => }
  }

  it should "not compile when the picklers do not have the exact same type (super-sub-types)" in {
    """
      |val pA = Pickle.from(new A())
      |val pB = Pickle.from(new B())
      |pA.orElse(pB)
    """.stripMargin shouldNot compile
  }

  it should "compile when the (second) subtype's pickler type is aligned by (for example) `upcast`" in {
    """
      |val pA = Pickle.from(new A())
      |val pB = Pickle.from(new B())
      |pA.orElse(pB.upcast[A])
    """.stripMargin should compile
  }

  it should "not compile when the picklers do not have the exact same type (sub-super-types)" in {
    """
      |val pA = Pickle.from(new A())
      |val pB = Pickle.from(new B())
      |pB.orElse(pA)
    """.stripMargin shouldNot compile
  }

  it should "compile when the (first) subtype's pickler type is aligned by (for example) `upcast`" in {
    """
      |val pA = Pickle.from(new A())
      |val pB = Pickle.from(new B())
      |pB.upcast[A].orElse(pA)
    """.stripMargin should compile
  }

  it should "not compile when the picklers do not have the exact same type (both sub-types)" in {
    """
      |val pB = Pickle.from(new B())
      |val pC = Pickle.from(new C())
      |pB.orElse(pC)
    """.stripMargin shouldNot compile
  }

  it should "compile when the picklers' types are aligned by (for example) `upcast`" in {
    """
      |val pB = Pickle.from(new B())
      |val pC = Pickle.from(new C())
      |pB.upcast[A].orElse(pC.upcast[A])
    """.stripMargin should compile
  }

  "satisfy" should "run the original pickler if the input satisfies the predicate" in {
    point.satisfy(_ % 2 == 0).serialize(2, "34") should matchPattern { case Success("234") => }
  }

  it should "fail if the predicate does not satisfy" in {
    val expectedMsg = "input '1' did not satisfy predicate"

    point.satisfy(_ % 2 == 0).serialize(1, "234") should matchPattern {
      case Failure(SerializerFailedException(`expectedMsg`)) =>
    }
  }

  "noneOf" should "succeed if the input does not contain any elements in the given list" in {
    point.noneOf(Seq(2, 3, 4)).serialize(1, "234") should matchPattern { case Success("1234") => }
  }

  it should "fail if the input is an element contained in the given list" in {
    val expectedMsg = "input '1' is contained in [1, 2, 3]"

    point.noneOf(Seq(1, 2, 3)).serialize(1, "234") should matchPattern {
      case Failure(SerializerFailedException(`expectedMsg`)) =>
    }
  }

  "maybe" should "run the original Pickler when the input is a Some" in {
    def sideEffect(boolean: Boolean): Unit = if (!boolean) fail("this position should not be visited")

    pointWithSideEffect(() => sideEffect(true))
      .maybe
      .serialize(Some(2), "345") should matchPattern { case Success("2345") => }
  }

  it should "not run the original Pickler when the input is a None" in {
    pointWithSideEffect(() => fail("this position should not be visited"))
      .maybe
      .serialize(None, "345") should matchPattern { case Success("345") => }
  }

  "many" should "apply the original Pickler as many times as the input list is long" in {
    point.many.serialize(List(1, 2, 3), "45") should matchPattern { case Success("12345") => }
  }

  it should "not apply and return the initial state when the input list is empty" in {
    point.many.serialize(Nil, "45") should matchPattern { case Success("45") => }
  }

  it should "stop the pickler as soon as it fails" in {
    val err = new Exception("err")
    val count = new AtomicInteger()
    pointWithSideEffect(() => { if (count.incrementAndGet() == 3) throw err })
      .many
      .serialize(List(1, 2, 3, 4, 5), "67") should matchPattern { case Failure(`err`) => }
    count.compareAndSet(3, 3) shouldBe true
  }

  "atLeastOnce" should "apply the original Pickler as many times as the input list is long" in {
    point.atLeastOnce.serialize(List(1, 2, 3), "45") should matchPattern { case Success("12345") => }
  }

  it should "succeed if the input list has only one element" in {
    point.atLeastOnce.serialize(List(1), "23") should matchPattern { case Success("123") => }
  }

  it should "fail on an empty input" in {
    point.atLeastOnce.serialize(Nil, "45") should matchPattern { case Failure(_: NoSuchElementException) => }
  }

  "takeUntil" should "succeed if all elements in the input list do satisfy the negate of the given predicate" in {
    point.takeUntil(_ % 2 == 0).serialize(List(1, 3, 5), "abc") should matchPattern { case Success("135abc") => }
  }

  it should "fail if any of the elements in the input list do satisfy the negate of the given predicate" in {
    val expectedMsg = "input '6' did not satisfy predicate"
    point.takeUntil(_ % 2 == 0).serialize(List(1, 3, 5, 6), "abc") should matchPattern { case Failure(SerializerFailedException(`expectedMsg`)) => }
  }

  "takeWhile" should "succeed if all elements in the input list do satisfy the given predicate" in {
    point.takeWhile(_ % 2 == 0).serialize(List(2, 4, 6), "abc") should matchPattern { case Success("246abc") => }
  }

  it should "fail if any of the elements in the input list do satisfy the given predicate" in {
    val expectedMsg = "input '7' did not satisfy predicate"
    point.takeWhile(_ % 2 == 0).serialize(List(2, 4, 6, 7), "abc") should matchPattern { case Failure(SerializerFailedException(`expectedMsg`)) => }
  }

  "separatedBy" should "construct a string with the separator between the characters" in {
    point.separatedBy('-')(StringPickle.char('-')).serialize(List(1, 2, 3), "abc") should matchPattern { case Success("1-2-3abc") => }
  }

  it should "return the initial state when the input list is empty" in {
    point.separatedBy('-')(StringPickle.char('-')).serialize(Nil, "abc") should matchPattern { case Success("abc") => }
  }

  "separatedBy1" should "construct a string with the separator between the characters" in {
    point.separatedBy1('-')(StringPickle.char('-')).serialize(List(1, 2, 3), "abc") should matchPattern { case Success("1-2-3abc") => }
  }

  it should "fail when the input list is empty" in {
    point.separatedBy1('-')(StringPickle.char('-')).serialize(Nil, "abc") should matchPattern { case Failure(_: NoSuchElementException) => }
  }
}
