package com.github.rvanheest.spickle.test.parser.xml

import org.scalatest.{ FlatSpec, Inside, Matchers }
import com.github.rvanheest.spickle.parser.xml.XmlParser._

import scala.util.{ Failure, Success }
import scala.xml.{ NamespaceBinding, TopScope, Utility }

class XmlParserTest extends FlatSpec with Matchers with Inside {

  private val foo = <foo>test</foo>
  private val bar = <bar/>
  private val baz = <baz>hello world</baz>

  "nodeWithName" should "consume the first node in the sequence and return it if its label is equal to the given String" in {
    inside(nodeWithName("foo").run(Seq(foo, bar, baz))) {
      case (Success(node), nodes) =>
        node shouldBe <foo>test</foo>
        nodes should (have size 2 and contain inOrderOnly(bar, baz))
    }
  }

  it should "consume the first node in the sequence and return an error when the label does not match the given String" in {
    inside(nodeWithName("bar").run(Seq(foo, bar, baz))) {
      case (Failure(e), nodes) =>
        e shouldBe a[NoSuchElementException]
        e.getMessage shouldBe "could not find an element with name 'bar'"
        nodes should (have size 3 and contain inOrderOnly(foo, bar, baz))
    }
  }

  it should "return an error when the input is empty" in {
    inside(nodeWithName("foo").run(Seq.empty)) {
      case (Failure(e), nodes) =>
        e shouldBe a[NoSuchElementException]
        e.getMessage shouldBe "can't parse an empty node sequence"
        nodes shouldBe empty
    }
  }

  "xmlToString" should "consume the first node in the sequence if it has the given label and return the text that is in it" in {
    inside(xmlToString("foo").run(Seq(foo, bar, baz))) {
      case (Success(s), nodes) =>
        s shouldBe foo.text
        nodes should (have size 2 and contain inOrderOnly(bar, baz))
    }
  }

  it should "consume the first node in the sequence if it has the given label and return an empty String if the node has no content" in {
    inside(xmlToString("bar").run(Seq(bar, baz))) {
      case (Success(s), nodes) =>
        s shouldBe empty
        nodes should (have size 1 and contain only baz)
    }
  }

  it should "consume the first node in the sequence if it has the given label and return a concattenated String of all subnodes when this node is not a 'leave'" in {
    // @formatter:off
    inside(xmlToString("qux").run(Seq(<qux><foo>hello</foo><bar>world</bar></qux>, foo))) {
    // @formatter:on
      case (Success(s), nodes) =>
        s shouldBe "helloworld"
        nodes should (have size 1 and contain only foo)
    }
  }

  "node" should "transform the string inside a node into something else using the given function" in {
    inside(node("foo")(s => s.length).run(Seq(foo, bar, baz))) {
      case (Success(n), nodes) =>
        n shouldBe 4
        nodes should (have size 2 and contain inOrderOnly(bar, baz))
    }
  }

  it should "catch any exceptions thrown inside the function and return them as the result of run" in {
    val error = new Exception("--error--")
    inside(node("foo")(_ => throw error).run(Seq(foo, bar, baz))) {
      case (Failure(e), nodes) =>
        e shouldBe error
        nodes should (have size 2 and contain inOrderOnly(bar, baz))
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
      bars <- xmlToString("bar").many
      baz <- xmlToString("baz")
    } yield bars.mkString(" ") + baz

    inside(branchNode("foo")(subParser).run(Utility.trim(input))) {
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
      bars <- xmlToString("bar").many
    } yield bars.mkString(" ")

    inside(branchNode("foo")(subParser).run(Utility.trim(input))) {
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
      bars <- xmlToString("bar").many
    } yield bars.mkString(" ")

    inside(branchNode("foo")(subParser).run(input map Utility.trim)) {
      case (Success(res), nodes) =>
        res shouldBe "hello world"
        // flattened structure in second foo is intentional; this was flattened in the Utility.trim above
        // @formatter:off
        nodes shouldBe Seq(<baz>!</baz>, <foo><bar>hello</bar><bar>world</bar><baz>!</baz></foo>)
        // @formatter:on
    }
  }

  "attribute" should "read the attribute with a given name from the first node in the input, transform it using the given constructor function and keep the attribute in the remaining node" in {
    // @formatter:off
    val input = <foo test="123" hello="abc">bar</foo>
    // @formatter:on
    inside(attribute("test")(_.toInt).run(input)) {
      case (Success(n), remainder) =>
        n shouldBe 123
        remainder shouldBe input
    }
  }

  it should "fail when the first node in the input does not contain an attribute with the given name" in {
    // @formatter:off
    val input = <foo test="123" hello="abc">bar</foo>
    // @formatter:on
    inside(attribute("tset")(_.toInt).run(input)) {
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
    inside(attribute("test")(_.toInt).run(input)) {
      case (Failure(e), remainder) =>
        e shouldBe a[NoSuchElementException]
        e.getMessage shouldBe "empty parser"
        remainder shouldBe input
    }
  }

  it should "fail when the input is empty" in {
    inside(attribute("tset")(_.toInt).run(Seq.empty)) {
      case (Failure(e), remainder) =>
        e shouldBe a[NoSuchElementException]
        e.getMessage shouldBe "you're trying to parse an attribute in an empty xml Node"
        remainder shouldBe empty
    }
  }

  it should "catch any exception that is thrown in the constructor function" in {
    // @formatter:off
    val input = <foo test="123" hello="abc">bar</foo>
    // @formatter:on
    val error = new Exception("--error--")
    inside(attribute("test")(_ => throw error).run(input)) {
      case (Failure(e), remainder) =>
        e shouldBe error
        remainder shouldBe input
    }
  }

  "attributeId" should "return the exact text that was found in the attribute" in {
    // @formatter:off
    val input = <foo test="123" hello="abc">bar</foo>
    // @formatter:on
    inside(attributeId("test").run(input)) {
      case (Success(n), remainder) =>
        n shouldBe "123"
        remainder shouldBe input
    }
  }

  "namespaceAttribute" should "parse a namespaced attribute" in {
    // @formatter:off
    val input = <foo xlink:type="simple" hello="abc">bar</foo>
    // @formatter:on
    implicit val ns = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    inside(namespaceAttribute("type").run(input)) {
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
    inside(namespaceAttribute("type").run(input)) {
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
    inside(namespaceAttribute("type").run(input)) {
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
    inside(namespaceAttribute("type").run(input)) {
      case (Success(s), remainder) =>
        s shouldBe "simple"
        remainder shouldBe input
    }
  }
}
