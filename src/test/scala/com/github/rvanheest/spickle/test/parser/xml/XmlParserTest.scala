package com.github.rvanheest.spickle.test.parser.xml

import org.scalatest.{ FlatSpec, Inside, Matchers }
import com.github.rvanheest.spickle.parser.xml.XmlParser._

import scala.util.{ Failure, Success }

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
    inside(xmlToString("qux").run(Seq(<qux><foo>hello</foo><bar>world</bar></qux>, foo))) {
      case (Success(s), nodes) =>
        s shouldBe "helloworld"
        nodes should (have size 1 and contain only foo)
    }
  }

  // TODO continue testing for `node`
}
