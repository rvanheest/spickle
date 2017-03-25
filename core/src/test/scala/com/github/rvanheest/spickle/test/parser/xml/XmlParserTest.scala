package com.github.rvanheest.spickle.test.parser.xml

import org.scalatest.{ FlatSpec, Inside, Matchers }
import com.github.rvanheest.spickle.parser.xml.XmlParser._

import scala.util.{ Failure, Success }
import scala.xml.{ NamespaceBinding, TopScope, Utility }

class XmlParserTest extends FlatSpec with Matchers with Inside {

  private val foo = <foo>test</foo>
  private val bar = <bar/>
  private val baz = <baz>hello world</baz>

  "node" should "consume the first node in the sequence and return it if its label is equal to the given String" in {
    inside(node("foo").parse(Seq(foo, bar, baz))) {
      case (Success(node), nodes) =>
        node shouldBe <foo>test</foo>
        nodes should (have size 2 and contain inOrderOnly(bar, baz))
    }
  }

  it should "consume the first node in the sequence and return an error when the label does not match the given String" in {
    inside(node("bar").parse(Seq(foo, bar, baz))) {
      case (Failure(e), nodes) =>
        e shouldBe a[NoSuchElementException]
        e.getMessage shouldBe "could not find an element with name 'bar'"
        nodes should (have size 3 and contain inOrderOnly(foo, bar, baz))
    }
  }

  it should "return an error when the input is empty" in {
    inside(node("foo").parse(Seq.empty)) {
      case (Failure(e), nodes) =>
        e shouldBe a[NoSuchElementException]
        e.getMessage shouldBe "can't parse an empty node sequence"
        nodes shouldBe empty
    }
  }

  "nodeToString" should "consume the first node in the sequence if it has the given label and return the text that is in it" in {
    inside(nodeToString("foo").parse(Seq(foo, bar, baz))) {
      case (Success(s), nodes) =>
        s shouldBe foo.text
        nodes should (have size 2 and contain inOrderOnly(bar, baz))
    }
  }

  it should "consume the first node in the sequence if it has the given label and return an empty String if the node has no content" in {
    inside(nodeToString("bar").parse(Seq(bar, baz))) {
      case (Success(s), nodes) =>
        s shouldBe empty
        nodes should (have size 1 and contain only baz)
    }
  }

  it should "consume the first node in the sequence if it has the given label and return a concattenated String of all subnodes when this node is not a 'leave'" in {
    // @formatter:off
    inside(nodeToString("qux").parse(Seq(<qux><foo>hello</foo><bar>world</bar></qux>, foo))) {
    // @formatter:on
      case (Success(s), nodes) =>
        s shouldBe "helloworld"
        nodes should (have size 1 and contain only foo)
    }
  }

  "branchNode" should "apply the subParser to the content of the first node in the input if it matches the given name" in {
    val input =
    // @formatter:off
      <foo>
        <bar>hello</bar>
        <bar>world</bar>
        <baz>!</baz>
      </foo>
      // @formatter:on

    val subParser = for {
      bars <- nodeToString("bar").many
      baz <- nodeToString("baz")
    } yield bars.mkString(" ") + baz

    inside(branchNode("foo")(subParser).parse(Utility.trim(input))) {
      case (Success(res), nodes) =>
        res shouldBe "hello world!"
        nodes shouldBe empty
    }
  }

  it should "apply the subParser to the content of the first node in the input if it matches the given name and leave any unparsed content from the subparser in the rest-input of the outer parser" in {
    val input =
    // @formatter:off
      <foo>
        <bar>hello</bar>
        <bar>world</bar>
        <baz>!</baz>
      </foo>
      // @formatter:on

    val subParser = for {
      bars <- nodeToString("bar").many
    } yield bars.mkString(" ")

    inside(branchNode("foo")(subParser).parse(Utility.trim(input))) {
      case (Success(res), nodes) =>
        res shouldBe "hello world"
        nodes shouldBe Seq(<baz>!</baz>)
    }
  }

  it should "apply the subParser to the content of the first node in the input if it matches the given name and leave any unparsed content from the outer parser in the rest-input of the outer parser" in {
    val input = Seq(
      // @formatter:off
      <foo>
        <bar>hello</bar>
        <bar>world</bar>
        <baz>!</baz>
      </foo>,
      <foo>
        <bar>hello</bar>
        <bar>world</bar>
        <baz>!</baz>
      </foo>
      // @formatter:on
    )

    val subParser = for {
      bars <- nodeToString("bar").many
    } yield bars.mkString(" ")

    inside(branchNode("foo")(subParser).parse(input map Utility.trim)) {
      case (Success(res), nodes) =>
        res shouldBe "hello world"
        // flattened structure in second foo is intentional; this was flattened in the Utility.trim above
        // @formatter:off
        nodes shouldBe Seq(<baz>!</baz>, <foo><bar>hello</bar><bar>world</bar><baz>!</baz></foo>)
        // @formatter:on
    }
  }

  "attribute" should "read the attribute with a given name from the first node in the input and keep the attribute in the remaining node" in {
    // @formatter:off
    val input = <foo test="123" hello="abc">bar</foo>
    // @formatter:on
    inside(attribute("test").parse(input)) {
      case (Success(s), remainder) =>
        s shouldBe "123"
        remainder shouldBe input
    }
  }

  it should "fail when the first node in the input does not contain an attribute with the given name" in {
    // @formatter:off
    val input = <foo test="123" hello="abc">bar</foo>
    // @formatter:on
    inside(attribute("tset").parse(input)) {
      case (Failure(e), remainder) =>
        e shouldBe a[NoSuchElementException]
        e.getMessage shouldBe "empty parser"
        remainder shouldBe input
    }
  }

  it should "fail when the attribute has an empty value" in {
    // @formatter:off
    val input = <foo test="" hello="abc">bar</foo>
    // @formatter:on
    inside(attribute("test").parse(input)) {
      case (Failure(e), remainder) =>
        e shouldBe a[NoSuchElementException]
        e.getMessage shouldBe "empty parser"
        remainder shouldBe input
    }
  }

  it should "fail when the input is empty" in {
    inside(attribute("tset").parse(Seq.empty)) {
      case (Failure(e), remainder) =>
        e shouldBe a[NoSuchElementException]
        e.getMessage shouldBe "you're trying to parse an attribute in an empty xml Node"
        remainder shouldBe empty
    }
  }

  "namespaceAttribute" should "parse a namespaced attribute" in {
    // @formatter:off
    val input = <foo xlink:type="simple" hello="abc">bar</foo>
    // @formatter:on
    implicit val ns = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    inside(namespaceAttribute("type").parse(input)) {
      case (Success(s), remainder) =>
        s shouldBe "simple"
        remainder shouldBe input
    }
  }

  it should "fail if the attribute does not have the proper namespace" in {
    // @formatter:off
    val input = <foo ylink:type="simple" hello="abc">bar</foo>
    // @formatter:on
    implicit val ns = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    inside(namespaceAttribute("type").parse(input)) {
      case (Failure(e), remainder) =>
        e shouldBe a[NoSuchElementException]
        e.getMessage shouldBe "empty parser"
        remainder shouldBe input
    }
  }

  it should "fail if the attribute isn't there at all" in {
    // @formatter:off
    val input = <foo hello="abc">bar</foo>
    // @formatter:on
    implicit val ns = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    inside(namespaceAttribute("type").parse(input)) {
      case (Failure(e), remainder) =>
        e shouldBe a[NoSuchElementException]
        e.getMessage shouldBe "empty parser"
        remainder shouldBe input
    }
  }

  it should "take the first when two the same attributes are in the node" in {
    // @formatter:off
    val input = <foo xlink:type="simple" xlink:href="#bar" hello="abc">bar</foo>
    // @formatter:on
    implicit val ns = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    inside(namespaceAttribute("type").parse(input)) {
      case (Success(s), remainder) =>
        s shouldBe "simple"
        remainder shouldBe input
    }
  }
}
