package com.github.rvanheest.spickle.test.serializer.xml

import com.github.rvanheest.spickle.serializer.SerializerFailedException
import com.github.rvanheest.spickle.serializer.xml.XmlSerializer._
import com.github.rvanheest.spickle.test.XmlEquality
import org.scalatest.{ FlatSpec, Inside, Matchers }

import scala.util.{ Failure, Success }
import scala.xml.{ Comment, NamespaceBinding, TopScope }

class XmlSerializerTest extends FlatSpec with Matchers with Inside with XmlEquality {

  private val xlinkNamespace: NamespaceBinding = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)

  "emptyNode" should "generate an empty node with the given name" in {
    val expectedResult = <foo/>
    emptyNode("foo").serialize(()) should equalTrimmed(expectedResult)
  }

  "emptyNode with namespace" should "generate an empty node with the given name and namespace" in {
    val expectedResult = <xlink:foo/>
    emptyNode("foo", xlinkNamespace).serialize(()) should equalTrimmed(expectedResult)
  }

  "node" should "append the input xml to the output if it matches the given label name" in {
    val expectedResult = Seq(<foo>test1</foo>, <bar>test2</bar>)
    node("foo").serialize(<foo>test1</foo>, Seq(<bar>test2</bar>)) should equalTrimmed(expectedResult)
  }

  it should "fail when the input xml has a different label than the given name" in {
    val input = <foo>test1</foo>
    inside(node("not-foo").serialize(input, Seq(<bar>test2</bar>))) {
      case Failure(e: NoSuchElementException) =>
        e should have message s"element '$input' does not contain an element with name 'not-foo'"
    }
  }

  "node with namespace" should "append the input xml to the output if it matches the given label name" in {
    val input1 = <xlink:foo xmlns:xlink="http://www.w3.org/1999/xlink">test1</xlink:foo>
    val input2 = <bar>test2</bar>

    node("foo", xlinkNamespace).serialize(input1, Seq(input2)) should equalTrimmed(Seq(input1, input2))
  }

  it should "fail when the input xml has a different label than the given name" in {
    val input = <xlink:foo xmlns:xlink="http://www.w3.org/1999/xlink">test1</xlink:foo>
    inside(node("not-foo", xlinkNamespace).serialize(input, Seq(<bar>test2</bar>))) {
      case Failure(e: NoSuchElementException) =>
        val namespace = "xmlns:xlink=\"http://www.w3.org/1999/xlink\""
        e should have message s"element '$input' does not contain an element with name 'not-foo' and namespace '$namespace'"
    }
  }

  it should "fail when the input xml has a different namespace than the given one" in {
    val input = <foo>test1</foo>.copy(prefix = "dc")
    inside(node("foo", xlinkNamespace).serialize(input, Seq(<bar>test2</bar>))) {
      case Failure(e: NoSuchElementException) =>
        val namespace = "xmlns:xlink=\"http://www.w3.org/1999/xlink\""
        e should have message s"element '$input' does not contain an element with name 'foo' and namespace '$namespace'"
    }
  }

  it should "fail when the input xml has no namespace, while some namespace was provided" in {
    val input = <foo>test1</foo>
    inside(node("foo", xlinkNamespace).serialize(input, Seq(<bar>test2</bar>))) {
      case Failure(e: NoSuchElementException) =>
        val namespace = "xmlns:xlink=\"http://www.w3.org/1999/xlink\""
        e should have message s"element '$input' does not contain an element with name 'foo' and namespace '$namespace'"
    }
  }

  "stringNode" should "create an xml node with the given content" in {
    val expectedResult = <foo>test1</foo>
    stringNode("foo").serialize("test1") should equalTrimmed(expectedResult)
  }

  it should "create an empty node when the input is an empty string" in {
    val expectedResult = <foo></foo>
    stringNode("foo").serialize("") should equalTrimmed(expectedResult)
  }

  "stringNode with namespace" should "create an xml node with the given content and namespace" in {
    val expectedResult = <xlink:foo>test1</xlink:foo>
    stringNode("foo", xlinkNamespace).serialize("test1") should equalTrimmed(expectedResult)
  }

  it should "create an empty node when the input is an empty string" in {
    val expectedResult = <xlink:foo></xlink:foo>
    stringNode("foo", xlinkNamespace).serialize("") should equalTrimmed(expectedResult)
  }

  "branchNode" should "create an xml tree with an outer node 'foo' and an inner node 'bar'" in {
    val expectedResult = <foo><bar>test1</bar></foo>
    branchNode("foo")(stringNode("bar")).serialize("test1") should equalTrimmed(expectedResult)
  }

  it should "create an xml tree with an outer node 'foo' and an inner node 'bar' with namespace" in {
    val expectedResult = <foo><xlink:bar>test1</xlink:bar></foo>
    branchNode("foo")(stringNode("bar", xlinkNamespace)).serialize("test1") should equalTrimmed(expectedResult)
  }

  "branchNode with namespace" should "create an xml tree with an outer node 'foo' with namespace and an inner node 'bar'" in {
    val expectedResult = <xlink:foo><bar>test1</bar></xlink:foo>
    branchNode("foo", xlinkNamespace)(stringNode("bar")).serialize("test1") should equalTrimmed(expectedResult)
  }

  it should "create an xml tree with an outer node 'foo' with namespace and an inner node 'bar' with namespace" in {
    val expectedResult = <xlink:foo><xlink:bar>test1</xlink:bar></xlink:foo>
    branchNode("foo", xlinkNamespace)(stringNode("bar", xlinkNamespace)).serialize("test1") should equalTrimmed(expectedResult)
  }

  it should "create an xml tree with an outer node 'foo' with namespace and an inner node 'bar' with namespace and an inner-inner node 'baz' with namespace" in {
    val expectedResult = <xlink:foo><xlink:bar><xlink:baz>test1</xlink:baz></xlink:bar></xlink:foo>
    branchNode("foo", xlinkNamespace)(branchNode("bar", xlinkNamespace)(stringNode("baz", xlinkNamespace))).serialize("test1") should equalTrimmed(expectedResult)
  }

  "attribute" should "add an attribute to the node in the initial state with the given label and the data from the input" in {
    val input = <foo hello="abc">bar</foo>
    val output = <foo test="123" hello="abc">bar</foo>

    attribute("test").serialize("123", input) should equalTrimmed(output)
  }

  it should "append an attribute to a node" in {
    val expectedResult = <foo bar="test1">test2</foo>

    attribute("bar").contramap[(String, String)] { case (left, _) => left }
      .combine(stringNode("foo").contramap { case (_, right) => right })
      .serialize(("test1", "test2")) should equalTrimmed(expectedResult)
  }

  it should "append multiple attributes to a node" in {
    val expectedResult = <foo bar="test1" baz="test2">test3</foo>

    attribute("bar").contramap[(String, String, String)] { case (left, _, _) => left }
      .combine(attribute("baz").contramap { case (_, middle, _) => middle })
      .combine(stringNode("foo").contramap { case (_, _, right) => right })
      .serialize(("test1", "test2", "test3")) should equalTrimmed(expectedResult)
  }

  it should "override existing attributes" in {
    val input = <foo test="hello" hello="abc">bar</foo>
    val output = <foo test="123" hello="abc">bar</foo>
    attribute("test").serialize("123", input) should equalTrimmed(output)
  }

  it should "fail when there is only a comment to append the attribute to" in {
    attribute("bar").serialize("test1", Comment("hello world")) should matchPattern {
      case Failure(SerializerFailedException("Can only add an attribute with name 'bar' to elements: <!--hello world-->")) =>
    }
  }

  it should "fail when there is no node to append the attribute to" in {
    attribute("bar").serialize("test1") should matchPattern {
      case Failure(SerializerFailedException("Cannot add an attribute with name 'bar' to an empty node sequence")) =>
    }
  }

  "attribute with namespace" should "add an attribute to the node in the initial state with the given label and the data from the input" in {
    // @formatter:off
    val input = <foo hello="abc">bar</foo>
    val output = <foo xlink:test="123" hello="abc">bar</foo>
    // @formatter:on

    attribute("test", xlinkNamespace).serialize("123", input) should equalTrimmed(output)
  }

  it should "append an attribute to a node" in {
    val expectedResult = <foo xlink:bar="test1">test2</foo>
    attribute("bar", xlinkNamespace).contramap[(String, String)] { case (left, _) => left }
      .combine(stringNode("foo").contramap { case (_, right) => right })
      .serialize(("test1", "test2")) should equalTrimmed(expectedResult)
  }

  it should "append multiple attributes to a node" in {
    val expectedResult = <foo xlink:bar="test1" xlink:baz="test2">test3</foo>

    attribute("bar", xlinkNamespace).contramap[(String, String, String)] { case (left, _, _) => left }
      .combine(attribute("baz", xlinkNamespace).contramap { case (_, middle, _) => middle })
      .combine(stringNode("foo").contramap { case (_, _, right) => right })
      .serialize(("test1", "test2", "test3")) should equalTrimmed(expectedResult)
  }

  it should "override existing attributes" in {
    // @formatter:off
    val input = <foo xlink:test="hello" hello="abc">bar</foo>
    val output = <foo xlink:test="123" hello="abc">bar</foo>
    // @formatter:on
    attribute("test", xlinkNamespace).serialize("123", input) should equalTrimmed(output)
  }

  it should "fail when there is only a comment to append the attribute to" in {
    attribute("bar", xlinkNamespace).serialize("test1", Comment("hello world")) should matchPattern {
      case Failure(SerializerFailedException("Can only add an attribute with name 'xlink:bar' to elements: <!--hello world-->")) =>
    }
  }

  it should "fail when there is no node to append the attribute to" in {
    attribute("bar", xlinkNamespace).serialize("test1") should matchPattern {
      case Failure(SerializerFailedException("Cannot add an attribute with name 'xlink:bar' to an empty node sequence")) =>
    }
  }

  "withNamespace" should "add a namespace to the node serialized with the given serializer" in {
    val expectedResult = <foo xmlns:xlink="http://www.w3.org/1999/xlink"><xlink:bar>test1</xlink:bar><xlink:bar>test2</xlink:bar></foo>

    val barSerializer = stringNode("bar", xlinkNamespace)
    val fooSerializer = branchNode("foo")(barSerializer.many)
    val serializer = withNamespace(xlinkNamespace)(fooSerializer)

    serializer.serialize(Seq("test1", "test2")) should equalTrimmed(expectedResult)
  }

  it should "add a namespace to a subnode in the tree" in {
    val expectedResult = <foo><xlink:bar xmlns:xlink="http://www.w3.org/1999/xlink">test1</xlink:bar><xlink:bar xmlns:xlink="http://www.w3.org/1999/xlink">test2</xlink:bar></foo>

    val barSerializer = withNamespace(xlinkNamespace)(stringNode("bar", xlinkNamespace))
    val serializer = branchNode("foo")(barSerializer.many)

    serializer.serialize(Seq("test1", "test2")) should equalTrimmed(expectedResult)
  }

  it should "add a namespace to every node in the sequence produced by the given serializer" in {
    val expectedResult = <foo><xlink:bar xmlns:xlink="http://www.w3.org/1999/xlink">test1</xlink:bar><xlink:bar xmlns:xlink="http://www.w3.org/1999/xlink">test2</xlink:bar></foo>

    val barSerializer = stringNode("bar", xlinkNamespace)
    val serializer = branchNode("foo")(withNamespace(xlinkNamespace)(barSerializer.many))

    serializer.serialize(Seq("test1", "test2")) should equalTrimmed(expectedResult)
  }

  "collect" should "convert all input one-by-one and in order according to the given serializer" in {
    val input = Seq("test1", "test2", "test3", "test4")
    val serializer = collect(stringNode("abc"))

    inside(serializer.serialize(input)) {
      case Success(xml) =>
        xml should contain inOrderOnly(
          <abc>test1</abc>,
          <abc>test2</abc>,
          <abc>test3</abc>,
          <abc>test4</abc>
        )
    }
  }

  it should "compose with other collect operators" in {
    val input = (Seq("test1", "test2", "test3", "test4"), Seq("random1", "random3"))

    val abcSerializer = collect(stringNode("abc"))
    val defSerializer = collect(stringNode("def"))
    val serializer = abcSerializer.contramap[(Seq[String], Seq[String])] { case (seq1, _) => seq1 }
      .combine(defSerializer.contramap { case (_, seq2) => seq2 })

    inside(serializer.serialize(input)) {
      case Success(xml) =>
        xml should contain inOrderOnly(
          <abc>test1</abc>,
          <abc>test2</abc>,
          <abc>test3</abc>,
          <abc>test4</abc>,
          <def>random1</def>,
          <def>random3</def>
        )
    }
  }

  it should "convert an empty list into an empty sequence of nodes" in {
    val input = Seq.empty[String]
    val serializer = collect(stringNode("abc"))

    serializer.serialize(input) should matchPattern { case Success(Seq()) => }
  }
}
