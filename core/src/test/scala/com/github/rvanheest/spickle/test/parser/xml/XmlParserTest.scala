package com.github.rvanheest.spickle.test.parser.xml

import com.github.rvanheest.spickle.parser.ParserFailedException
import com.github.rvanheest.spickle.parser.xml.XmlParser
import com.github.rvanheest.spickle.parser.xml.XmlParser._
import org.scalatest.{ FlatSpec, Inside, Matchers }
import shapeless.Generic

import scala.util.{ Failure, Success }
import scala.xml.{ NamespaceBinding, TopScope, Utility }

class XmlParserTest extends FlatSpec with Matchers with Inside {

  private val xlinkNamespace: NamespaceBinding = NamespaceBinding("xlink", "http://www.w3.org/1999/xlink", TopScope)

  private val foo = <foo>test</foo>
  private val fooPf = <xlink:foo>test</xlink:foo>
  private val fooNS = <xlink:foo xmlns:xlink="http://www.w3.org/1999/xlink">test</xlink:foo>
  private val bar = <bar/>
  private val barNS = <xlink:bar xmlns:xlink="http://www.w3.org/1999/xlink"/>
  private val baz = <baz>hello world</baz>

  "node" should "consume the first node in the sequence and return it if its label is equal to the given String" in {
    node("foo").parse(Seq(foo, bar, baz)) should matchPattern {
      case (Success(`foo`), Seq(`bar`, `baz`)) =>
    }
  }

  it should "not consume the first node in the sequence and return an error when the label does not match the given String" in {
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

  "node with namespace" should "consume the first node in the sequence and return it if its label and namespace are equal to the given String and NamespaceBinding" in {
    node("foo", xlinkNamespace).parse(Seq(fooPf, bar, baz)) should matchPattern {
      case (Success(`fooPf`), Seq(`bar`, `baz`)) =>
    }
  }

  it should "not consume the first node in the sequence and return an error when the label does not match the given String" in {
    val expectedMsg = "could not find an element with name 'bar' and namespace 'xmlns:xlink=\"http://www.w3.org/1999/xlink\"'"
    node("bar", xlinkNamespace).parse(Seq(fooPf, bar, baz)) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), Seq(`fooPf`, `bar`, `baz`)) =>
    }
  }

  it should "not consume the first node in the sequence and return an error when the namespace does not match the given NamespaceBinding" in {
    val expectedMsg = "could not find an element with name 'foo' and namespace 'xmlns:xlink=\"http://www.w3.org/1999/xlink\"'"
    node("foo", xlinkNamespace).parse(Seq(<other:foo>test</other:foo>, bar, baz)) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), Seq(<other:foo>test</other:foo>, `bar`, `baz`)) =>
    }
  }

  it should "return an error when the input is empty" in {
    val expectedMsg = "can't parse an empty node sequence"
    node("foo", xlinkNamespace).parse(Seq.empty) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), Nil) =>
    }
  }

  "emptyNode" should "consume a the first node in the sequence if it has the given label and return Unit" in {
    emptyNode("foo").parse(Seq(foo, bar, baz)) should matchPattern {
      case (Success(()), Seq(`bar`, `baz`)) =>
    }
  }

  it should "not consume the first node in the sequence and return an error when the label does not match the given String" in {
    val expectedMsg = "could not find an element with name 'bar'"
    emptyNode("bar").parse(Seq(foo, bar, baz)) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), Seq(`foo`, `bar`, `baz`)) =>
    }
  }

  it should "return an error when the input is empty" in {
    val expectedMsg = "can't parse an empty node sequence"
    emptyNode("foo").parse(Seq.empty) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), Nil) =>
    }
  }

  "emptyNode with namespace" should "consume the first node in the sequence if it has the given label and namespace and return Unit" in {
    emptyNode("foo", xlinkNamespace).parse(Seq(fooPf, bar, baz)) should matchPattern {
      case (Success(()), Seq(`bar`, `baz`)) =>
    }
  }

  it should "not consume the first node in the sequence and return an error when the label does not match the given String" in {
    val expectedMsg = "could not find an element with name 'bar' and namespace 'xmlns:xlink=\"http://www.w3.org/1999/xlink\"'"
    emptyNode("bar", xlinkNamespace).parse(Seq(fooPf, bar, baz)) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), Seq(`fooPf`, `bar`, `baz`)) =>
    }
  }

  it should "not consume the first node in the sequence and return an error when the namespace does not match the given NamespaceBinding" in {
    val expectedMsg = "could not find an element with name 'foo' and namespace 'xmlns:xlink=\"http://www.w3.org/1999/xlink\"'"
    emptyNode("foo", xlinkNamespace).parse(Seq(<other:foo>test</other:foo>, bar, baz)) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), Seq(<other:foo>test</other:foo>, `bar`, `baz`)) =>
    }
  }

  it should "return an error when the input is empty" in {
    val expectedMsg = "can't parse an empty node sequence"
    emptyNode("foo", xlinkNamespace).parse(Seq.empty) should matchPattern {
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

  "stringNode with namespace" should "consume the first node in the sequence if it has the given label and return the text that is in it" in {
    val expectedOutput = fooPf.text
    stringNode("foo", xlinkNamespace).parse(Seq(fooPf, bar, baz)) should matchPattern {
      case (Success(`expectedOutput`), Seq(`bar`, `baz`)) =>
    }
  }

  it should "consume the first node in the sequence if it has the given label and return an empty String if the node has no content" in {
    stringNode("bar", xlinkNamespace).parse(Seq(barNS, baz)) should matchPattern {
      case (Success(""), Seq(`baz`)) =>
    }
  }

  it should "consume the first node in the sequence if it has the given label and return a concattenated String of all subnodes when this node is not a 'leave'" in {
    // @formatter:off
    stringNode("qux", xlinkNamespace).parse(Seq(<xlink:qux xmlns:xlink="http://www.w3.org/1999/xlink"><foo>hello</foo><bar>world</bar></xlink:qux>, foo)) should matchPattern {
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

  it should "fail when the parent parser is not fulfilled" in {
    val input = Utility.trim(
      // @formatter:off
      <foo>
        <bar>hello</bar>
        <bar>world</bar>
        <baz>!</baz>
      </foo>
      // @formatter:on
    )

    val subParser = for {
      bars <- stringNode("bar").many
      baz <- stringNode("baz")
    } yield bars.mkString(" ") + baz

    branchNode("not-foo")(subParser).parse(input) should matchPattern {
      case (Failure(ParserFailedException("could not find an element with name 'not-foo'")), Seq(`input`)) =>
    }
  }

  "branchNode with namespace" should "apply the subParser to the content of the first node in the input if it matches the given name" in {
    val input =
    // @formatter:off
      <xlink:foo xmlns:xlink="http://www.w3.org/1999/xlink">
        <bar>hello</bar>
        <bar>world</bar>
        <baz>!</baz>
      </xlink:foo>
      // @formatter:on

    val subParser = for {
      bars <- stringNode("bar").many
      baz <- stringNode("baz")
    } yield bars.mkString(" ") + baz

    branchNode("foo", xlinkNamespace)(subParser).parse(Utility.trim(input)) should matchPattern {
      case (Success("hello world!"), Nil) =>
    }
  }

  it should "apply the subParser to the content of the first node in the input if it matches the given name and leave any unparsed content from the subparser in the rest-input of the outer parser" in {
    val input =
    // @formatter:off
      <xlink:foo xmlns:xlink="http://www.w3.org/1999/xlink">
        <bar>hello</bar>
        <bar>world</bar>
        <baz>!</baz>
      </xlink:foo>
      // @formatter:on

    val subParser = for {
      bars <- stringNode("bar").many
    } yield bars.mkString(" ")

    branchNode("foo", xlinkNamespace)(subParser).parse(Utility.trim(input)) should matchPattern {
      case (Success("hello world"), Nil) =>
    }
  }

  it should "apply the subParser to the content of the first node in the input if it matches the given name and leave any unparsed content from the outer parser in the rest-input of the outer parser" in {
    val input = Seq(
      // @formatter:off
      <xlink:foo xmlns:xlink="http://www.w3.org/1999/xlink">
        <bar>hello</bar>
        <bar>world</bar>
        <baz>!</baz>
      </xlink:foo>,
      <xlink:foo xmlns:xlink="http://www.w3.org/1999/xlink">
        <bar>hello2</bar>
        <bar>world2</bar>
        <baz>!2</baz>
      </xlink:foo>
      // @formatter:on
    )

    val subParser = for {
      bars <- stringNode("bar").many
    } yield bars.mkString(" ")

    branchNode("foo", xlinkNamespace)(subParser).parse(input map Utility.trim) should matchPattern {
      // @formatter:off
      case (Success("hello world"), Seq(<foo><bar>hello2</bar><bar>world2</bar><baz>!2</baz></foo>)) =>
      // flattened structure in second foo is intentional; this was flattened in the Utility.trim above
      // @formatter:on
    }
  }

  it should "fail when the parent parser is not fulfilled" in {
    val input = Utility.trim(
    // @formatter:off
      <xlink:foo xmlns:xlink="http://www.w3.org/1999/xlink">
        <bar>hello</bar>
        <bar>world</bar>
        <baz>!</baz>
      </xlink:foo>
      // @formatter:on
    )

    val subParser = for {
      bars <- stringNode("bar").many
      baz <- stringNode("baz")
    } yield bars.mkString(" ") + baz

    branchNode("not-foo", xlinkNamespace)(subParser).parse(input) should matchPattern {
      case (Failure(ParserFailedException("could not find an element with name 'not-foo' and namespace 'xmlns:xlink=\"http://www.w3.org/1999/xlink\"'")), Seq(`input`)) =>
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

  "attribute with namespace" should "parse a namespaced attribute" in {
    // @formatter:off
    val input = <foo xlink:type="simple" hello="abc">bar</foo>
    // @formatter:on
    attribute("type", xlinkNamespace).parse(input) should matchPattern {
      case (Success("simple"), `input`) =>
    }
  }

  it should "fail if the attribute does not have the proper namespace" in {
    // @formatter:off
    val input = <foo ylink:type="simple" hello="abc">bar</foo>
    // @formatter:on
    val expectedMsg = "attribute 'type' with namespace ' xmlns:xlink=\"http://www.w3.org/1999/xlink\"' is not found"

    attribute("type", xlinkNamespace).parse(input) should matchPattern {
      case (Failure(ParserFailedException(`expectedMsg`)), `input`) =>
    }
  }

  it should "fail if the attribute isn't there at all" in {
    // @formatter:off
    val input = <foo hello="abc">bar</foo>
    // @formatter:on
    val expectedMsg = "attribute 'type' with namespace ' xmlns:xlink=\"http://www.w3.org/1999/xlink\"' is not found"

    attribute("type", xlinkNamespace).parse(input) should matchPattern {
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

  "collect" should "parse all nodes that satisfy the given parser, and leave the others in the remaining input" in {
    val input = Seq(
      <abc>test1</abc>,
      <def>random1</def>,
      <abc>test2</abc>,
      <ghi>random2</ghi>,
      <klm>random3</klm>,
      <abc>test3</abc>,
      <abc>test4</abc>
    )
    val parser = collect(stringNode("abc"))

    val (result, remainder) = parser.parse(input)
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
    val parser = for {
      abcs <- collect(stringNode("abc"))
      defs <- collect(stringNode("def"))
    } yield (abcs, defs)

    val (result, remainder) = parser.parse(input)
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
    val parser = collect(stringNode("unknown-node"))

    val (result, remainder) = parser.parse(input)
    result should matchPattern { case Success(Seq()) => }
    remainder shouldBe input
  }
}
