package com.github.rvanheest.spickle.test.pickle.xml

import com.github.rvanheest.spickle.pickle.PickleFailedException
import com.github.rvanheest.spickle.pickle.xml.XmlPickle
import com.github.rvanheest.spickle.pickle.xml.XmlPickle._
import org.scalatest.{ FlatSpec, Inside, Matchers }

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

    inside(branchNode("foo")(subPickle).serialize(foo, NodeSeq.Empty)) {
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
    attribute("test").serialize("123", NodeSeq.Empty) should matchPattern {
      case Failure(PickleFailedException("Cannot add an attribute with name 'test' to an empty node sequence")) =>
    }
  }

  it should "fail if the first node in the state is not an element" in {
    attribute("test").serialize("123", Comment("hello")) should matchPattern {
      case Failure(PickleFailedException("Can only add an attribute with name 'test' to elements: <!--hello-->")) =>
    }
  }

  "namespaceAttribute" should "add a namespace attribute to the node in the initial state with the given label and the data from the input" in {
    // @formatter:off
    val input = <foo hello="abc">bar</foo>
    val output = <foo xlink:type="simple" hello="abc">bar</foo>
    // @formatter:on
    implicit val ns = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    namespaceAttribute("type").serialize("simple", input) should matchPattern {
      case Success(Seq(`output`)) =>
    }
  }

  it should "override existing attributes" in {
    // @formatter:off
    val input = <foo xlink:type="abc" hello="abc">bar</foo>
    val output = <foo xlink:type="simple" hello="abc">bar</foo>
    // @formatter:on
    implicit val ns = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    namespaceAttribute("type").serialize("simple", input) should matchPattern {
      case Success(Seq(`output`)) =>
    }
  }

  it should "fail when the state is empty" in {
    implicit val ns = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    namespaceAttribute("type").serialize("simple", NodeSeq.Empty) should matchPattern {
      case Failure(PickleFailedException("Cannot add an attribute with name 'xlink:type' to an empty node sequence")) =>
    }
  }

  it should "fail if the first node in the state is not an element" in {
    implicit val ns = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    namespaceAttribute("type").serialize("simple", Comment("hello")) should matchPattern {
      case Failure(PickleFailedException("Can only add an attribute with name 'xlink:type' to elements: <!--hello-->")) =>
    }
  }

  "all2" should "turn an all of (present) optional objects to xml" in {
    val abcPickle = stringNode("abc")
    val defPickle = stringNode("def")
    val combined = XmlPickle.all(abcPickle, defPickle)(optional, optional)
    val fooPickle = branchNode("foo")(combined)

    val output = <foo><abc>blabla</abc><def>albalb</def></foo>
    fooPickle.serialize((Some("blabla"), Some("albalb")), NodeSeq.Empty) should matchPattern {
      case Success(Seq(`output`)) =>
    }
  }

  it should "turn an all of (present) optional and mandatory objects to xml" in {
    val abcPickle = stringNode("abc")
    val defPickle = stringNode("def")
    val combined = XmlPickle.all(abcPickle, defPickle)(optional, mandatory)
    val fooPickle = branchNode("foo")(combined)

    val output = <foo><abc>blabla</abc><def>albalb</def></foo>
    fooPickle.serialize((Some("blabla"), "albalb"), NodeSeq.Empty) should matchPattern {
      case Success(Seq(`output`)) =>
    }
  }

  it should "turn an all of (absent) optional and mandatory objects to xml" in {
    val abcPickle = stringNode("abc")
    val defPickle = stringNode("def")
    val combined = XmlPickle.all(abcPickle, defPickle)(optional, mandatory)
    val fooPickle = branchNode("foo")(combined)

    val output = <foo><def>albalb</def></foo>
    fooPickle.serialize((None, "albalb"), NodeSeq.Empty) should matchPattern {
      case Success(Seq(`output`)) =>
    }
  }

  it should "turn an all of mandatory objects to xml" in {
    val abcPickle = stringNode("abc")
    val defPickle = stringNode("def")
    val combined = XmlPickle.all(abcPickle, defPickle)(mandatory, mandatory)
    val fooPickle = branchNode("foo")(combined)

    val output = <foo><abc>blabla</abc><def>albalb</def></foo>
    fooPickle.serialize(("blabla", "albalb"), NodeSeq.Empty) should matchPattern {
      case Success(Seq(`output`)) =>
    }
  }

  it should "turn an all of (absent) optional objects to xml" in {
    val abcPickle = stringNode("abc")
    val defPickle = stringNode("def")
    val combined = XmlPickle.all(abcPickle, defPickle)(optional, optional)
    val fooPickle = branchNode("foo")(combined)

    val output = <foo></foo>
    fooPickle.serialize((None, None), NodeSeq.Empty) should matchPattern {
      case Success(Seq(`output`)) =>
    }
  }
}
