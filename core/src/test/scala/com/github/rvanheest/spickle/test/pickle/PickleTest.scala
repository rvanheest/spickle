package com.github.rvanheest.spickle.test.pickle

import com.github.rvanheest.spickle.parser.{ Parser, ParserFailedException }
import com.github.rvanheest.spickle.pickle.string.StringPickle
import com.github.rvanheest.spickle.pickle.{ Pickle, PickleFailedException }
import org.scalatest.{ FlatSpec, Matchers }

import scala.util.{ Failure, Success, Try }

class PickleTest extends FlatSpec with Matchers {

  private val emptyError = ParserFailedException("you're trying to parse a character in an empty String")

  private type TestPickle = Pickle[String, Int]

  private def point: TestPickle = pointWithSideEffect(() => ())

  private def pointWithSideEffect[U](sideEffect: () => U): TestPickle = new TestPickle(
    pickler = (n, s) => Try {
      sideEffect()
      n + s
    },
    parser = Parser(s => {
      sideEffect()
      s.toList match {
        case Nil => (Failure(emptyError), "")
        case p :: ps => (Try(p.toString.toInt), ps.mkString)
      }
    }))

  "pickle" should "run the pickler function in the Pickler" in {
    point.pickle(1, "23") should matchPattern { case Success("123") => }
  }

  "parse" should "run the parser function in the Pickler" in {
    point.parse("123") should matchPattern { case (Success(1), "23") => }
  }

  case class IntWrapper(i: Int)

  "seq map" should "unpack the integer and concat it to the rest of the input" in {
    point.seq[IntWrapper](_.i)
      .map(IntWrapper)
      .pickle(IntWrapper(1), "234") should matchPattern { case Success("1234") => }
  }

  it should "pack the first character up into the IntWrapper" in {
    point.seq[IntWrapper](_.i)
      .map(IntWrapper)
      .parse("1234") should matchPattern { case (Success(IntWrapper(1)), "234") => }
  }

  it should "not use the unpack function while parsing" in {
    var notVisited = true
    point.seq[IntWrapper](w => { notVisited = false; w.i })
      .map(IntWrapper)
      .parse("1234")._1 shouldBe a[Success[_]]

    notVisited shouldBe true
  }

  it should "fail if the unpack function throws an exception" in {
    val err = new Exception("err")
    point.seq[IntWrapper](_ => throw err)
      .map(IntWrapper)
      .pickle(IntWrapper(1), "234") should matchPattern { case Failure(`err`) => }
  }

  it should "not execute the original Pickler if the unpack function throws an exception" in {
    var notVisited = true

    pointWithSideEffect(() => notVisited = false)
      .seq[IntWrapper](_ => throw new Exception("err"))
      .map(IntWrapper)
      .pickle(IntWrapper(1), "234") shouldBe a[Failure[_]]

    notVisited shouldBe true
  }

  it should "fail if the original Pickler fails" in {
    val err = new Exception("err")
    pointWithSideEffect(() => throw err)
      .seq[IntWrapper](_.i)
      .map(IntWrapper)
      .pickle(IntWrapper(1), "234") should matchPattern { case Failure(`err`) => }
  }

  it should "not use the map function when pickling" in {
    var notVisited = true

    point.seq[IntWrapper](_.i)
      .map(i => { notVisited = false; IntWrapper(i) })
      .pickle(IntWrapper(1), "234") shouldBe a[Success[_]]

    notVisited shouldBe true
  }

  case class IntTupleWrapper(i1: Int, i2: Int)

  "seq flatMap" should "unpack the integers and concat them to the rest of the input" in {
    point.seq[IntTupleWrapper](_.i1)
      .flatMap(i1 => point.seq[IntTupleWrapper](_.i2).map(IntTupleWrapper(i1, _)))
      .pickle(IntTupleWrapper(1, 2), "34") should matchPattern { case Success("1234") => }
  }

  it should "pack the first two characters up into the IntTupleWrapper" in {
    point.seq[IntTupleWrapper](_.i1)
      .flatMap(i1 => point.seq[IntTupleWrapper](_.i2).map(IntTupleWrapper(i1, _)))
      .parse("1234") should matchPattern { case (Success(IntTupleWrapper(1, 2)), "34") => }
  }

  it should "not use the unpack functions while parsing" in {
    var notVisited1 = true
    var notVisited2 = true

    point.seq[IntTupleWrapper](w => { notVisited1 = false; w.i1 })
      .flatMap(i1 => point
        .seq[IntTupleWrapper](w => { notVisited2 = false; w.i2 })
        .map(IntTupleWrapper(i1, _)))
      .parse("1234")._1 shouldBe a[Success[_]]

    notVisited1 shouldBe true
    notVisited2 shouldBe true
  }

  it should "fail if the unpack function throws an exception" in {
    val err = new Exception("err")
    point.seq[IntTupleWrapper](w => { throw err; w.i1 })
      .flatMap(i1 => point.seq[IntTupleWrapper](_.i2).map(IntTupleWrapper(i1, _)))
      .pickle(IntTupleWrapper(1, 2), "34") should matchPattern { case Failure(`err`) => }
  }

  it should "not execute the flatMap function if the unpack function throws an exception" in {
    val err = new Exception("err")
    var notVisited = true
    point.seq[IntTupleWrapper](w => { throw err; w.i1 })
      .flatMap(i1 => {
        notVisited = false
        point.seq[IntTupleWrapper](_.i2).map(IntTupleWrapper(i1, _))
      })
      .pickle(IntTupleWrapper(1, 2), "34") shouldBe a[Failure[_]]

    notVisited shouldBe true
  }

  it should "not execute the original Pickler if the unpack function throws an exception" in {
    val err = new Exception("err")
    var notVisited = true
    pointWithSideEffect(() => notVisited = false)
      .seq[IntTupleWrapper](w => { throw err; w.i1 })
      .flatMap(i1 => point.seq[IntTupleWrapper](_.i2).map(IntTupleWrapper(i1, _)))
      .pickle(IntTupleWrapper(1, 2), "34") shouldBe a[Failure[_]]

    notVisited shouldBe true
  }

  it should "fail if the flatMap function throws an exception" in {
    val err = new Exception("err")
    point.seq[IntTupleWrapper](_.i1)
      .flatMap(i1 => {
        throw err
        point.seq[IntTupleWrapper](_.i2).map(IntTupleWrapper(i1, _))
      })
      .pickle(IntTupleWrapper(1, 2), "34") should matchPattern { case Failure(`err`) => }
  }

  it should "not execute the original Pickler if the flatMap function throws an exception" in {
    val err = new Exception("err")
    var notVisited = true
    pointWithSideEffect(() => notVisited = false).seq[IntTupleWrapper](_.i1)
      .flatMap(i1 => {
        throw err
        point.seq[IntTupleWrapper](_.i2).map(IntTupleWrapper(i1, _))
      })
      .pickle(IntTupleWrapper(1, 2), "34") shouldBe a[Failure[_]]

    notVisited shouldBe true
  }

  it should "fail if the original Pickler fails" in {
    val err = new Exception("err")
    pointWithSideEffect(() => throw err).seq[IntTupleWrapper](_.i1)
      .flatMap(i1 => point.seq[IntTupleWrapper](_.i2)
        .map(IntTupleWrapper(i1, _)))
      .pickle(IntTupleWrapper(1, 2), "34") should matchPattern { case Failure(`err`) => }
  }

  class A()
  class B() extends A()
  class C() extends A()

  "upcast" should "cast a type to a given supertype" in {
    val pickleB: Pickle[String, B] = Pickle.from(new B())
    val pickleA: Pickle[String, A] = pickleB.upcast[A]
    pickleA.pickle(new B(), "abc") shouldBe a[Success[_]]
  }

  it should "fail in casting if the pickler has an input of a different type" in {
    import scala.reflect.classTag
    val pickleB: Pickle[String, B] = Pickle.from(new B())
    val pickleA: Pickle[String, A] = pickleB.upcast[A]
    val expectedMsg = s"can't cast ${ classOf[C] } to ${ classTag[B] }"
    pickleA.pickle(new C(), "abc") should matchPattern {
      case Failure(PickleFailedException(`expectedMsg`)) =>
    }
  }

  "orElse" should "use the first pickler if this one succeeds" in {
    point.orElse(Pickle.from(1)).pickle(1, "23") should matchPattern { case Success("123") => }
  }

  it should "not use the second pickler if the first one succeeds" in {
    var notVisited = true
    lazy val p2 = pointWithSideEffect(() => notVisited = false)
    point.orElse(p2).pickle(1, "23") shouldBe a[Success[_]]

    notVisited shouldBe true
  }

  it should "use the second pickler if the first one fails" in {
    val err = new Exception("err")
    pointWithSideEffect(() => throw err)
      .orElse(Pickle.from(99))
      .pickle(1, "23") should matchPattern { case Success("23") => }
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
    point.satisfy(_ % 2 == 0).pickle(2, "34") should matchPattern { case Success("234") => }
  }

  it should "fail if the predicate does not satisfy" in {
    val expectedMsg = "input '1' did not satisfy predicate"

    point.satisfy(_ % 2 == 0).pickle(1, "234") should matchPattern {
      case Failure(PickleFailedException(`expectedMsg`)) =>
    }
  }

  "noneOf" should "succeed if the input does not contain any elements in the given list" in {
    point.noneOf(Seq(2, 3, 4)).pickle(1, "234") should matchPattern { case Success("1234") => }
  }

  it should "fail if the input is an element contained in the given list" in {
    val expectedMsg = "input '1' is contained in [1, 2, 3]"

    point.noneOf(Seq(1, 2, 3)).pickle(1, "234") should matchPattern {
      case Failure(PickleFailedException(`expectedMsg`)) =>
    }
  }

  "maybe" should "run the original Pickler when the input is a Some" in {
    var visited = false

    pointWithSideEffect(() => visited = true)
      .maybe
      .pickle(Some(2), "345") should matchPattern { case Success("2345") => }
    visited shouldBe true
  }

  it should "not run the original Pickler when the input is a None" in {
    var notVisited = true

    pointWithSideEffect(() => notVisited = false)
      .maybe
      .pickle(None, "345") should matchPattern { case Success("345") => }
    notVisited shouldBe true
  }

  "many" should "apply the original Pickler as many times as the input list is long" in {
    point.many.pickle(List(1, 2, 3), "45") should matchPattern { case Success("12345") => }
  }

  it should "not apply and return the initial state when the input list is empty" in {
    point.many.pickle(Nil, "45") should matchPattern { case Success("45") => }
  }

  it should "stop the pickler as soon as it fails" in {
    val err = new Exception("err")
    var count = 0
    pointWithSideEffect(() => { count += 1; if (count == 3) throw err })
      .many
      .pickle(List(1, 2, 3, 4, 5), "67") should matchPattern { case Failure(`err`) => }
    count shouldBe 3
  }

  "atLeastOnce" should "apply the original Pickler as many times as the input list is long" in {
    point.atLeastOnce.pickle(List(1, 2, 3), "45") should matchPattern { case Success("12345") => }
  }

  it should "succeed if the input list has only one element" in {
    point.atLeastOnce.pickle(List(1), "23") should matchPattern { case Success("123") => }
  }

  it should "fail on an empty input" in {
    point.atLeastOnce.pickle(Nil, "45") should matchPattern { case Failure(_: NoSuchElementException) => }
  }

  "takeUntil" should "succeed if all elements in the input list do satisfy the negate of the given predicate" in {
    point.takeUntil(_ % 2 == 0).pickle(List(1, 3, 5), "abc") should matchPattern { case Success("135abc") => }
  }

  it should "fail if any of the elements in the input list do satisfy the negate of the given predicate" in {
    val expectedMsg = "input '6' did not satisfy predicate"
    point.takeUntil(_ % 2 == 0).pickle(List(1, 3, 5, 6), "abc") should matchPattern { case Failure(PickleFailedException(`expectedMsg`)) => }
  }

  "takeWhile" should "succeed if all elements in the input list do satisfy the given predicate" in {
    point.takeWhile(_ % 2 == 0).pickle(List(2, 4, 6), "abc") should matchPattern { case Success("246abc") => }
  }

  it should "fail if any of the elements in the input list do satisfy the given predicate" in {
    val expectedMsg = "input '7' did not satisfy predicate"
    point.takeWhile(_ % 2 == 0).pickle(List(2, 4, 6, 7), "abc") should matchPattern { case Failure(PickleFailedException(`expectedMsg`)) => }
  }

  "separatedBy" should "construct a string with the separator between the characters" in {
    point.separatedBy('-')(StringPickle.char('-')).pickle(List(1, 2, 3), "abc") should matchPattern { case Success("1-2-3abc") => }
  }

  it should "return the initial state when the input list is empty" in {
    point.separatedBy('-')(StringPickle.char('-')).pickle(Nil, "abc") should matchPattern { case Success("abc") => }
  }

  "separatedBy1" should "construct a string with the separator between the characters" in {
    point.separatedBy1('-')(StringPickle.char('-')).pickle(List(1, 2, 3), "abc") should matchPattern { case Success("1-2-3abc") => }
  }

  it should "fail when the input list is empty" in {
    point.separatedBy1('-')(StringPickle.char('-')).pickle(Nil, "abc") should matchPattern { case Failure(_: NoSuchElementException) => }
  }
}
