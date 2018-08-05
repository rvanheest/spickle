package com.github.rvanheest.spickle.test.parser.xml

import com.github.rvanheest.spickle.parser.ParserFailedException
import com.github.rvanheest.spickle.parser.xml.XmlParser
import com.github.rvanheest.spickle.parser.xml.XmlParser.{ stringNode, _ }
import org.scalatest.{ FlatSpec, Inside, Matchers }
import shapeless.Generic

import scala.util.{ Failure, Success }
import scala.xml.{ NamespaceBinding, TopScope, Utility }

class XmlParserTest extends FlatSpec with Matchers with Inside {

  private val foo = <foo>test</foo>
  private val bar = <bar/>
  private val baz = <baz>hello world</baz>

  "node" should "consume the first node in the sequence and return it if its label is equal to the given String" in {
    node("foo").parse(Seq(foo, bar, baz)) should matchPattern {
      case (Success(`foo`), Seq(`bar`, `baz`)) =>
    }
  }

  it should "consume the first node in the sequence and return an error when the label does not match the given String" in {
    val expectedMsg = "could not find an element with name 'bar'"
    node("bar").parse(Seq(foo, bar, baz)) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), Seq(`foo`, `bar`, `baz`)) =>
    }
  }

  it should "return an error when the input is empty" in {
    val expectedMsg = "can't parse an empty node sequence"
    node("foo").parse(Seq.empty) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), Nil) =>
    }
  }

  "stringNode" should "consume the first node in the sequence if it has the given label and return the text that is in it" in {
    val expectedOutput = foo.text
    stringNode("foo").parse(Seq(foo, bar, baz)) should matchPattern {
      case (Success(`expectedOutput`), Seq(`bar`, `baz`)) =>
    }
  }

  it should "consume the first node in the sequence if it has the given label and return an empty String if the node has no content" in {
    stringNode("bar").parse(Seq(bar, baz)) should matchPattern {
      case (Success(""), Seq(`baz`)) =>
    }
  }

  it should "consume the first node in the sequence if it has the given label and return a concattenated String of all subnodes when this node is not a 'leave'" in {
    // @formatter:off
    stringNode("qux").parse(Seq(<qux><foo>hello</foo><bar>world</bar></qux>, foo)) should matchPattern {
    // @formatter:on
      case (Success("helloworld"), Seq(`foo`)) =>
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
      bars <- stringNode("bar").many
      baz <- stringNode("baz")
    } yield bars.mkString(" ") + baz

    branchNode("foo")(subParser).parse(Utility.trim(input)) should matchPattern {
      case (Success("hello world!"), Nil) =>
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
      bars <- stringNode("bar").many
    } yield bars.mkString(" ")

    branchNode("foo")(subParser).parse(Utility.trim(input)) should matchPattern {
      case (Success("hello world"), Nil) =>
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
        <bar>hello2</bar>
        <bar>world2</bar>
        <baz>!2</baz>
      </foo>
      // @formatter:on
    )

    val subParser = for {
      bars <- stringNode("bar").many
    } yield bars.mkString(" ")

    branchNode("foo")(subParser).parse(input map Utility.trim) should matchPattern {
      // @formatter:off
      case (Success("hello world"), Seq(<foo><bar>hello2</bar><bar>world2</bar><baz>!2</baz></foo>)) =>
      // flattened structure in second foo is intentional; this was flattened in the Utility.trim above
      // @formatter:on
    }
  }

  "attribute" should "read the attribute with a given name from the first node in the input and keep the attribute in the remaining node" in {
    // @formatter:off
    val input = <foo test="123" hello="abc">bar</foo>
    // @formatter:on
    attribute("test").parse(input) should matchPattern { case (Success("123"), `input`) => }
  }

  it should "fail when the first node in the input does not contain an attribute with the given name" in {
    // @formatter:off
    val input = <foo test="123" hello="abc">bar</foo>
    // @formatter:on
    val expectedMsg = "attribute 'tset' is not found or is empty"

    attribute("tset").parse(input) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), `input`) =>
    }
  }

  it should "fail when the attribute has an empty value" in {
    // @formatter:off
    val input = <foo test="" hello="abc">bar</foo>
    // @formatter:on
    val expectedMsg = "attribute 'test' is not found or is empty"
    attribute("test").parse(input) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), `input`) =>
    }
  }

  it should "fail when the input is empty" in {
    val expectedMsg = "you're trying to parse an attribute in an empty xml Node"

    attribute("tset").parse(Seq.empty) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), Nil) =>
    }
  }

  "namespaceAttribute" should "parse a namespaced attribute" in {
    // @formatter:off
    val input = <foo xlink:type="simple" hello="abc">bar</foo>
    // @formatter:on
    implicit val ns: NamespaceBinding = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    namespaceAttribute("type").parse(input) should matchPattern {
      case (Success("simple"), `input`) =>
    }
  }

  it should "fail if the attribute does not have the proper namespace" in {
    // @formatter:off
    val input = <foo ylink:type="simple" hello="abc">bar</foo>
    // @formatter:on
    implicit val ns: NamespaceBinding = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    val expectedMsg = "attribute 'type' with namespace ' xmlns:xlink=\"http://www.w3.org/1999/xlink\"' is not found"

    namespaceAttribute("type").parse(input) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), `input`) =>
    }
  }

  it should "fail if the attribute isn't there at all" in {
    // @formatter:off
    val input = <foo hello="abc">bar</foo>
    // @formatter:on
    implicit val ns: NamespaceBinding = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)
    val expectedMsg = "attribute 'type' with namespace ' xmlns:xlink=\"http://www.w3.org/1999/xlink\"' is not found"

    namespaceAttribute("type").parse(input) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), `input`) =>
    }
  }

  "all2" should "parse a list of xml nodes in the order in which the parsers are defined" in {
    val input = Utility.trim(
      <foo>
        <abc>blabla</abc>
        <def>albalb</def>
      </foo>
    )

    val abcParser = stringNode("abc")
    val defParser = stringNode("def")
    val combined = XmlParser.fromAllOptional(abcParser)
      .andOptional(defParser)
      .build
      .map(_.reverse.tupled)
    val fooParser = branchNode("foo")(combined)

    val (result, remainder) = fooParser.parse(input)
    result should matchPattern { case Success((Some("blabla"), Some("albalb"))) => }
    remainder shouldBe empty
  }

  it should "parse a list of xml nodes in reversed order" in {
    val input = Utility.trim(
      <foo>
        <def>albalb</def>
        <abc>blabla</abc>
      </foo>
    )

    val abcParser = stringNode("abc")
    val defParser = stringNode("def")
    val combined = XmlParser.fromAllOptional(abcParser)
      .andOptional(defParser)
      .build
      .map(_.reverse.tupled)
    val fooParser = branchNode("foo")(combined)

    val (result, remainder) = fooParser.parse(input)
    result should matchPattern { case Success((Some("blabla"), Some("albalb"))) => }
    remainder shouldBe empty
  }

  it should "not fail when multiple elements of the same type are found" in {
    // the remaining elements with the same name might be part of another structure in the XSD
    val input = Utility.trim(
      <foo>
        <abc>blabla</abc>
        <def>albalb</def>
        <abc>xyzuvw</abc>
      </foo>
    )

    val abcParser = stringNode("abc")
    val defParser = stringNode("def")
    val combined = XmlParser.fromAllOptional(abcParser)
      .andOptional(defParser)
      .build
      .map(_.reverse.tupled)

    combined.parse(input.child) should matchPattern {
      case (Success((Some("blabla"), Some("albalb"))), Seq(<abc>xyzuvw</abc>)) =>
    }
  }

  it should "fail if a mandatory element is missing" in {
    val input = Utility.trim(
      <foo>
        <abc>blabla</abc>
      </foo>
    )

    val abcParser = stringNode("abc")
    val defParser = stringNode("def")
    val combined = XmlParser.fromAllMandatory(abcParser)
      .andMandatory(defParser)
      .build
      .map(_.reverse.tupled)
    val fooParser = branchNode("foo")(combined)

    val (result, remainder) = fooParser.parse(input)
    inside(result) {
      case Failure(ParserFailedException(msg)) =>
        msg shouldBe "missing mandatory element in any"
    }
    remainder shouldBe empty
  }

  "all3" should "parse a list of xml nodes in the order in which the parsers are defined" in {
    val input = Utility.trim(
      <foo>
        <abc>blabla</abc>
        <def>albalb</def>
        <ghi>xyzxyz</ghi>
      </foo>
    )

    val abcParser = stringNode("abc")
    val defParser = stringNode("def")
    val ghiParser = stringNode("ghi")
    val combined = XmlParser.fromAllOptional(abcParser)
      .andOptional(defParser)
      .andOptional(ghiParser)
      .build
      .map(_.reverse.tupled)
    val fooParser = branchNode("foo")(combined)

    val (result, remainder) = fooParser.parse(input)
    result should matchPattern { case Success((Some("blabla"), Some("albalb"), Some("xyzxyz"))) => }
    remainder shouldBe empty
  }

  "build" should "construct an object from the all builders" in {
    case class Foo(abc: String, `def`: String, ghi: Option[String])

    val input = Utility.trim(
      <foo>
        <abc>blabla</abc>
        <def>albalb</def>
        <ghi>xyzxyz</ghi>
      </foo>
    )

    val abcParser = stringNode("abc")
    val defParser = stringNode("def")
    val ghiParser = stringNode("ghi")
    val combined = XmlParser.fromAllOptional(ghiParser)
      .andMandatory(defParser)
      .andMandatory(abcParser)
      .build(Generic[Foo])
    val fooParser = branchNode("foo")(combined)

    val (result, remainder) = fooParser.parse(input)
    result should matchPattern { case Success(Foo("blabla", "albalb", Some("xyzxyz"))) => }
    remainder shouldBe empty
  }

  it should "construct an object from the all builders when the optional element is not present" in {
    case class Foo(abc: String, `def`: String, ghi: Option[String])

    val input = Utility.trim(
      <foo>
        <abc>blabla</abc>
        <def>albalb</def>
      </foo>
    )

    val abcParser = stringNode("abc")
    val defParser = stringNode("def")
    val ghiParser = stringNode("ghi")
    val combined = XmlParser.fromAllOptional(ghiParser)
      .andMandatory(defParser)
      .andMandatory(abcParser)
      .build(Generic[Foo])
    val fooParser = branchNode("foo")(combined)

    val (result, remainder) = fooParser.parse(input)
    result should matchPattern { case Success(Foo("blabla", "albalb", None)) => }
    remainder shouldBe empty
  }

  it should "construct an object from the all builders when the declared order is reversed" in {
    case class Foo(abc: String, `def`: String, ghi: Option[String])

    val input = Utility.trim(
      <foo>
        <abc>blabla</abc>
        <def>albalb</def>
        <ghi>xyzxyz</ghi>
      </foo>
    )

    val abcParser = stringNode("abc")
    val defParser = stringNode("def")
    val ghiParser = stringNode("ghi")
    val combined = XmlParser.fromAllMandatory(abcParser)
      .andMandatory(defParser)
      .andOptional(ghiParser)
      .map(_.reverse)
      .build(Generic[Foo])
    val fooParser = branchNode("foo")(combined)

    val (result, remainder) = fooParser.parse(input)
    result should matchPattern { case Success(Foo("blabla", "albalb", Some("xyzxyz"))) => }
    remainder shouldBe empty
  }
}
