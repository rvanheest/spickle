package com.github.rvanheest.spickle.test.pickle.xml

import com.github.rvanheest.spickle.pickle.xml.XmlPickle
import com.github.rvanheest.spickle.pickle.xml.XmlPickle._
import com.github.rvanheest.spickle.serializer.SerializerFailedException
import org.scalatest.{ FlatSpec, Inside, Matchers }
import shapeless.HNil

import scala.util.{ Failure, Success }
import scala.xml._

class XmlPickleTest extends FlatSpec with Matchers with Inside {

  private val foo = <foo>test</foo>
  private val bar = <bar/>
  private val baz = <baz>hello world</baz>

  "emptyNode" should "create an empty xml element with the given label" in {
    emptyNode("bar").serialize((), Seq(foo, baz)) should matchPattern {
      case Success(Seq(`bar`, `foo`, `baz`)) =>
    }
  }

  "node" should "pickle the input node if its label is equal to the given String" in {
    node("foo").serialize(foo, Seq(bar, baz)) should matchPattern {
      case Success(Seq(`foo`, `bar`, `baz`)) =>
    }
  }

  it should "fail if the label of the input node does not match the given String" in {
    val expectedMsg = s"element '$foo' does not contain an element with name 'bar'"
    inside(node("bar").serialize(foo, Seq(bar, baz))) {
      case Failure(e: NoSuchElementException) => e.getMessage shouldBe expectedMsg
    }
  }

  "stringNode" should "pickle the input text and wrap it in an xml element with the given label" in {
    stringNode("foo").serialize(foo.text, Seq(bar, baz)) should matchPattern {
      case Success(Seq(`foo`, `bar`, `baz`)) =>
    }
  }

  "branchNode" should "apply the subPickle to the contents of the input and wrap it all in a node with the given label" in {
    val output =
    // @formatter:off
      <foo>
        <bar>hello</bar>
        <bar>world</bar>
        <baz>!</baz>
      </foo>
      // @formatter:on
      case class Foo(bars: Seq[Bar], baz: Baz)
    case class Bar(s: String)
    case class Baz(s: String)

    val subPickle = for {
      bars <- stringNode("bar").seq[Bar](_.s).map(Bar).many.seq[Foo](_.bars)
      baz <- stringNode("baz").seq[Baz](_.s).map(Baz).seq[Foo](_.baz)
    } yield Foo(bars, baz)

    val foo = Foo(Seq(Bar("hello"), Bar("world")), Baz("!"))

    inside(branchNode("foo")(subPickle).serialize(foo)) {
      case Success(out) => out.toString() shouldBe Utility.trim(output).toString()
    }
  }

  "attribute" should "add an attribute to the node in the initial state with the given label and the data from the input" in {
    // @formatter:off
    val input = <foo hello="abc">bar</foo>
    val output = <foo test="123" hello="abc">bar</foo>
    // @formatter:on
    attribute("test").serialize("123", input) should matchPattern { case Success(Seq(`output`)) => }
  }

  it should "override existing attributes" in {
    // @formatter:off
    val input = <foo test="hello" hello="abc">bar</foo>
    val output = <foo test="123" hello="abc">bar</foo>
    // @formatter:on
    attribute("test").serialize("123", input) should matchPattern {
      case Success(Seq(`output`)) =>
    }
  }

  it should "fail when the state is empty" in {
    attribute("test").serialize("123") should matchPattern {
      case Failure(SerializerFailedException("Cannot add an attribute with name 'test' to an empty node sequence")) =>
    }
  }

  it should "fail if the first node in the state is not an element" in {
    attribute("test").serialize("123", Comment("hello")) should matchPattern {
      case Failure(SerializerFailedException("Can only add an attribute with name 'test' to elements: <!--hello-->")) =>
    }
  }

  "namespaceAttribute" should "add a namespace attribute to the node in the initial state with the given label and the data from the input" in {
    // @formatter:off
    val input = <foo hello="abc">bar</foo>
    val output = <foo xlink:type="simple" hello="abc">bar</foo>
    // @formatter:on
    implicit val ns: NamespaceBinding = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    namespaceAttribute("type").serialize("simple", input) should matchPattern {
      case Success(Seq(`output`)) =>
    }
  }

  it should "override existing attributes" in {
    // @formatter:off
    val input = <foo xlink:type="abc" hello="abc">bar</foo>
    val output = <foo xlink:type="simple" hello="abc">bar</foo>
    // @formatter:on
    implicit val ns: NamespaceBinding = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    namespaceAttribute("type").serialize("simple", input) should matchPattern {
      case Success(Seq(`output`)) =>
    }
  }

  it should "fail when the state is empty" in {
    implicit val ns: NamespaceBinding = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    namespaceAttribute("type").serialize("simple") should matchPattern {
      case Failure(SerializerFailedException("Cannot add an attribute with name 'xlink:type' to an empty node sequence")) =>
    }
  }

  it should "fail if the first node in the state is not an element" in {
    implicit val ns: NamespaceBinding = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    namespaceAttribute("type").serialize("simple", Comment("hello")) should matchPattern {
      case Failure(SerializerFailedException("Can only add an attribute with name 'xlink:type' to elements: <!--hello-->")) =>
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

    val output = <foo><abc>blabla</abc><def>albalb</def></foo>
    fooPickle.serialize((Some("blabla"), Some("albalb"))) should matchPattern {
      case Success(Seq(`output`)) =>
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

    val output = <foo><abc>blabla</abc><def>albalb</def></foo>
    fooPickle.serialize((Some("blabla"), "albalb")) should matchPattern {
      case Success(Seq(`output`)) =>
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

    val output = <foo><def>albalb</def></foo>
    fooPickle.serialize((None, "albalb")) should matchPattern {
      case Success(Seq(`output`)) =>
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

    val output = <foo><abc>blabla</abc><def>albalb</def></foo>
    fooPickle.serialize(("blabla", "albalb")) should matchPattern {
      case Success(Seq(`output`)) =>
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

    val output = <foo></foo>
    fooPickle.serialize((None, None)) should matchPattern {
      case Success(Seq(`output`)) =>
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
    val pickle = collect(stringNode("abc"))

    val (result, remainder) = pickle.parse(input)
    result should matchPattern { case Success(Seq("test1", "test2", "test3", "test4")) => }
    remainder should contain only(
      <def>random1</def>,
      <ghi>random2</ghi>,
      <klm>random3</klm>,
    )
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
    type Output = (Seq[String], Seq[String])
    val pickle: XmlPickle[Output] = for {
      abcs <- collect(stringNode("abc")).seq[Output] { case (seq1, _) => seq1 }
      defs <- collect(stringNode("def")).seq[Output] { case (_, seq2) => seq2 }
    } yield (abcs, defs)

    val (result, remainder) = pickle.parse(input)
    result should matchPattern { case Success((Seq("test1", "test2", "test3", "test4"), Seq("random1", "random3"))) => }
    remainder should contain only <ghi>random2</ghi>
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

    val (result, remainder) = pickle.parse(input)
    result should matchPattern { case Success(Seq()) => }
    remainder shouldBe input
  }

  it should "convert all input one-by-one and in order according to the given serializer" in {
    val input = Seq("test1", "test2", "test3", "test4")
    val pickle = collect(stringNode("abc"))

    inside(pickle.serialize(input)) {
      case Success(xml) =>
        xml should contain inOrderOnly(
          <abc>test1</abc>,
          <abc>test2</abc>,
          <abc>test3</abc>,
          <abc>test4</abc>,
        )
    }
  }

  it should "compose with other collect operators" in {
    val input = (Seq("test1", "test2", "test3", "test4"), Seq("random1", "random3"))

    type Output = (Seq[String], Seq[String])
    val pickle: XmlPickle[Output] = for {
      abcs <- collect(stringNode("abc")).seq[Output] { case (seq1, _) => seq1 }
      defs <- collect(stringNode("def")).seq[Output] { case (_, seq2) => seq2 }
    } yield (abcs, defs)

    inside(pickle.serialize(input)) {
      case Success(xml) =>
        xml should contain inOrderOnly(
          <abc>test1</abc>,
          <abc>test2</abc>,
          <abc>test3</abc>,
          <abc>test4</abc>,
          <def>random1</def>,
          <def>random3</def>,
        )
    }
  }

  it should "convert an empty list into an empty sequence of nodes" in {
    val input = Seq.empty[String]
    val pickle = collect(stringNode("abc"))

    pickle.serialize(input) should matchPattern { case Success(Seq()) => }
  }

  it should "have sorted a xs-choice-many content after first parsing it, and then serializing it again" in {
    val input = Seq(
      <abc>test1</abc>,
      <def>random1</def>,
      <abc>test2</abc>,
      <ghi>random2</ghi>,
      <def>random3</def>,
      <abc>test3</abc>,
      <abc>test4</abc>
    )

    type Output = (Seq[String], Seq[String])
    val pickle: XmlPickle[Output] = for {
      abcs <- collect(stringNode("abc")).seq[Output] { case (seq1, _) => seq1 }
      defs <- collect(stringNode("def")).seq[Output] { case (_, seq2) => seq2 }
    } yield (abcs, defs)

    val (result, remainder) = pickle.parse(input)
    inside(result) {
      case Success(output) =>
        inside(pickle.serialize(output, remainder)) {
          case Success(outputXml) =>
            outputXml should contain inOrderOnly(
              <abc>test1</abc>,
              <abc>test2</abc>,
              <abc>test3</abc>,
              <abc>test4</abc>,
              <def>random1</def>,
              <def>random3</def>,
              <ghi>random2</ghi>,
            )
        }
    }
  }
}
