package com.github.rvanheest.spickle.test.pickle.xml

import com.github.rvanheest.spickle.pickle.xml.XmlPickle
import com.github.rvanheest.spickle.pickle.xml.XmlPickle.{ stringNode, _ }
import com.github.rvanheest.spickle.test.XmlEquality
import org.scalatest.{ FlatSpec, Inside, Matchers }
import shapeless.HNil

import scala.util.{ Success, Try }
import scala.xml._

class XmlPickleTest extends FlatSpec with Matchers with Inside with XmlEquality {

  private val xlinkNamespace: NamespaceBinding = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)

  private val foo = <foo>test</foo>
  private val fooPf = <xlink:foo>test</xlink:foo>
  private val bar = <bar/>
  private val barPf = <xlink:bar/>

  "emptyNode" should "create an empty xml element with the given label" in {
    val input = bar
    val pickle = emptyNode("bar")
    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  "emptyNode with namespace" should "create an empty xml element with the given label and prefix" in {
    val input = barPf
    val pickle = emptyNode("bar", xlinkNamespace)
    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  "node" should "pickle the input node if its label is equal to the given String" in {
    val input = foo
    val pickle = node("foo")
    inside(pickle.parse(input)) {
      case (result @ Success(parsedResult), remainder) =>
        remainder shouldBe empty
        result should equalTrimmed(input)

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  "node with namespace" should "pickle the input node if its label and prefix are equal to the given String and namespace" in {
    val input = fooPf
    val pickle = node("foo", xlinkNamespace)
    inside(pickle.parse(input)) {
      case (result @ Success(parsedResult), remainder) =>
        remainder shouldBe empty
        result should equalTrimmed(input)

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  "stringNode" should "pickle the input text and wrap it in an xml element with the given label" in {
    val input = foo
    val pickle = stringNode("foo")
    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty
        parsedResult shouldBe "test"

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  "stringNode with namespace" should "pickle the input text and wrap it in an xml element with the given label and prefix" in {
    val input = fooPf
    val pickle = stringNode("foo", xlinkNamespace)
    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty
        parsedResult shouldBe "test"

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  case class Foo(bars: Seq[Bar], baz: Baz)
  case class Bar(s: String)
  case class Baz(s: String)

  "branchNode" should "apply the subPickle to the contents of the input and wrap it all in a node with the given label" in {
    val input = Utility.trim(
      // @formatter:off
      <foo>
        <bar>hello</bar>
        <bar>world</bar>
        <baz>!</baz>
      </foo>
    // @formatter:on
    )
    val output = Foo(Seq(Bar("hello"), Bar("world")), Baz("!"))

    def barPickle: XmlPickle[Bar] = stringNode("bar").seq[Bar](_.s).map(Bar)

    def bazPickle: XmlPickle[Baz] = stringNode("baz").seq[Baz](_.s).map(Baz)

    def subPickle: XmlPickle[Foo] = for {
      bars <- barPickle.many.seq[Foo](_.bars)
      baz <- bazPickle.seq[Foo](_.baz)
    } yield Foo(bars, baz)

    val pickle = branchNode("foo")(subPickle)

    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty
        parsedResult shouldBe output

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  "branchNode with namespace" should "apply the subPickle to the contents of the input and wrap it all in a node with the given label and prefix" in {
    val dcNamespace = NamespaceBinding("dc", "http://purl.org/dc/elements/1.1/", TopScope)
    val input = Utility.trim(
      // @formatter:off
      <xlink:foo>
        <bar>hello</bar>
        <bar>world</bar>
        <dc:baz>!</dc:baz>
      </xlink:foo>
    // @formatter:on
    )
    val output = Foo(Seq(Bar("hello"), Bar("world")), Baz("!"))

    def barPickle: XmlPickle[Bar] = stringNode("bar").seq[Bar](_.s).map(Bar)

    def bazPickle: XmlPickle[Baz] = stringNode("baz", dcNamespace).seq[Baz](_.s).map(Baz)

    def subPickle: XmlPickle[Foo] = for {
      bars <- barPickle.many.seq[Foo](_.bars)
      baz <- bazPickle.seq[Foo](_.baz)
    } yield Foo(bars, baz)

    val pickle = branchNode("foo", xlinkNamespace)(subPickle)

    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty
        parsedResult shouldBe output

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  "attribute" should "pickle an attribute on a node with the given label" in {
    val input = <foo test="123" hello="abc">bar</foo>
    val pickle = attribute("test")

    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        Try { remainder } should equalTrimmed(input) // attributes are not taken out
        parsedResult shouldBe "123"

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  it should "pickle an attribute and branchNode" in {
    val input = <foo baz="abc"><bar>test1</bar><bar>test2</bar></foo>
    val output = Foo(Seq(Bar("test1"), Bar("test2")), Baz("abc"))
    val pickle: XmlPickle[Foo] = for {
      baz <- attribute("baz").seq[Baz](_.s).map(Baz).seq[Foo](_.baz)
      foo <- branchNode("foo") {
        for {
          bars <- stringNode("bar").seq[Bar](_.s).map(Bar).many.seq[Foo](_.bars)
        } yield Foo(bars, baz)
      }.seqId
    } yield foo

    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty
        parsedResult shouldBe output

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  "attribute with namespace" should "pickle an attribute on a node with the given label" in {
    val input = <foo xlink:test="123" hello="abc">bar</foo>
    val pickle = attribute("test", xlinkNamespace)

    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        Try { remainder } should equalTrimmed(input) // attributes are not taken out
        parsedResult shouldBe "123"

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  it should "pickle an attribute and branchNode" in {
    val input = <foo xlink:baz="abc"><bar>test1</bar><bar>test2</bar></foo>
    val output = Foo(Seq(Bar("test1"), Bar("test2")), Baz("abc"))
    val pickle: XmlPickle[Foo] = for {
      baz <- attribute("baz", xlinkNamespace).seq[Baz](_.s).map(Baz).seq[Foo](_.baz)
      foo <- branchNode("foo") {
        for {
          bars <- stringNode("bar").seq[Bar](_.s).map(Bar).many.seq[Foo](_.bars)
        } yield Foo(bars, baz)
      }.seqId
    } yield foo

    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty
        parsedResult shouldBe output

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  "withNamespace" should "pickle a namespace to an xml structure" in {
    val dcNamespace = NamespaceBinding("dc", "http://purl.org/dc/elements/1.1/", TopScope)
    val totalNamespace = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", NamespaceBinding("dc", "http://purl.org/dc/elements/1.1/", TopScope))
    val input = Utility.trim(
      // @formatter:off
      <xlink:foo xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:xlink="http://www.w3.org/1999/xlink">
        <bar>hello</bar>
        <bar>world</bar>
        <dc:baz>!</dc:baz>
      </xlink:foo>
    // @formatter:on
    )
    val output = Foo(Seq(Bar("hello"), Bar("world")), Baz("!"))

    def barPickle: XmlPickle[Bar] = stringNode("bar").seq[Bar](_.s).map(Bar)

    def bazPickle: XmlPickle[Baz] = stringNode("baz", dcNamespace).seq[Baz](_.s).map(Baz)

    def subPickle: XmlPickle[Foo] = for {
      bars <- barPickle.many.seq[Foo](_.bars)
      baz <- bazPickle.seq[Foo](_.baz)
    } yield Foo(bars, baz)

    val pickle = withNamespace(totalNamespace)(branchNode("foo", xlinkNamespace)(subPickle))

    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty
        parsedResult shouldBe output

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  "all2" should "turn an all of (present) optional objects to xml" in {
    val abcPickle = stringNode("abc")
    val defPickle = stringNode("def")
    val combined = XmlPickle.fromAllOptional(abcPickle)
      .andOptional(defPickle)
      .build
      .seq[(Option[String], Option[String])] { case (abcP, defP) => defP :: abcP :: HNil }
      .map(_.reverse.tupled)
    val fooPickle = branchNode("foo")(combined)

    val input = <foo><abc>blabla</abc><def>albalb</def></foo>
    inside(fooPickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty
        parsedResult shouldBe(Some("blabla"), Some("albalb"))

        val resultXml = fooPickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  it should "turn an all of (present) optional and mandatory objects to xml" in {
    val abcPickle = stringNode("abc")
    val defPickle = stringNode("def")
    val combined = XmlPickle.fromAllOptional(abcPickle)
      .andMandatory(defPickle)
      .build
      .seq[(Option[String], String)] { case (abcP, defP) => defP :: abcP :: HNil }
      .map(_.reverse.tupled)
    val fooPickle = branchNode("foo")(combined)

    val input = <foo><abc>blabla</abc><def>albalb</def></foo>
    inside(fooPickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty
        parsedResult shouldBe(Some("blabla"), "albalb")

        val resultXml = fooPickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  it should "turn an all of (absent) optional and mandatory objects to xml" in {
    val abcPickle = stringNode("abc")
    val defPickle = stringNode("def")
    val combined = XmlPickle.fromAllOptional(abcPickle)
      .andMandatory(defPickle)
      .build
      .seq[(Option[String], String)] { case (abcP, defP) => defP :: abcP :: HNil }
      .map(_.reverse.tupled)
    val fooPickle = branchNode("foo")(combined)

    val input = <foo><def>albalb</def></foo>
    inside(fooPickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty
        parsedResult shouldBe(None, "albalb")

        val resultXml = fooPickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  it should "turn an all of mandatory objects to xml" in {
    val abcPickle = stringNode("abc")
    val defPickle = stringNode("def")
    val combined = XmlPickle.fromAllMandatory(abcPickle)
      .andMandatory(defPickle)
      .build
      .seq[(String, String)] { case (abcP, defP) => defP :: abcP :: HNil }
      .map(_.reverse.tupled)
    val fooPickle = branchNode("foo")(combined)

    val input = <foo><abc>blabla</abc><def>albalb</def></foo>
    inside(fooPickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty
        parsedResult shouldBe("blabla", "albalb")

        val resultXml = fooPickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  it should "turn an all of (absent) optional objects to xml" in {
    val abcPickle = stringNode("abc")
    val defPickle = stringNode("def")
    val combined = XmlPickle.fromAllOptional(abcPickle)
      .andOptional(defPickle)
      .build
      .seq[(Option[String], Option[String])] { case (abcP, defP) => defP :: abcP :: HNil }
      .map(_.reverse.tupled)
    val fooPickle = branchNode("foo")(combined)

    val input = <foo></foo>
    inside(fooPickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder shouldBe empty
        parsedResult shouldBe(None, None)

        val resultXml = fooPickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }

  "collect" should "parse all nodes that satisfy the given parser" in {
    val input = Seq(
      <abc>test1</abc>,
      <def>random1</def>,
      <abc>test2</abc>,
      <ghi>random2</ghi>,
      <klm>random3</klm>,
      <abc>test3</abc>,
      <abc>test4</abc>
    )
    val output = Seq(
      <abc>test1</abc>,
      <abc>test2</abc>,
      <abc>test3</abc>,
      <abc>test4</abc>,
      <def>random1</def>,
      <ghi>random2</ghi>,
      <klm>random3</klm>
    )
    val pickle = collect(stringNode("abc"))

    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder should contain only(
          <def>random1</def>,
          <ghi>random2</ghi>,
          <klm>random3</klm>
        )
        parsedResult shouldBe Seq("test1", "test2", "test3", "test4")

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(output)
    }
  }

  it should "pass the remaining input to the next 'collect' parser" in {
    val input = Seq(
      <abc>test1</abc>,
      <def>random1</def>,
      <abc>test2</abc>,
      <ghi>random2</ghi>,
      <def>random3</def>,
      <abc>test3</abc>,
      <abc>test4</abc>
    )
    val output = Seq(
      <abc>test1</abc>,
      <abc>test2</abc>,
      <abc>test3</abc>,
      <abc>test4</abc>,
      <def>random1</def>,
      <def>random3</def>,
      <ghi>random2</ghi>
    )
    type Output = (Seq[String], Seq[String])
    val pickle: XmlPickle[Output] = for {
      abcs <- collect(stringNode("abc")).seq[Output] { case (seq1, _) => seq1 }
      defs <- collect(stringNode("def")).seq[Output] { case (_, seq2) => seq2 }
    } yield (abcs, defs)

    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder should contain only <ghi>random2</ghi>
        parsedResult shouldBe(Seq("test1", "test2", "test3", "test4"), Seq("random1", "random3"))

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(output)
    }
  }

  it should "don't collect any nodes if they all don't satisfy the parser" in {
    val input = Seq(
      <abc>test1</abc>,
      <def>random1</def>,
      <abc>test2</abc>,
      <ghi>random2</ghi>,
      <klm>random3</klm>,
      <abc>test3</abc>,
      <abc>test4</abc>
    )
    val pickle = collect(stringNode("unknown-node"))

    inside(pickle.parse(input)) {
      case (Success(parsedResult), remainder) =>
        remainder should contain theSameElementsInOrderAs input
        parsedResult shouldBe empty

        val resultXml = pickle.serialize(parsedResult, remainder)
        resultXml should equalTrimmed(input)
    }
  }
}
