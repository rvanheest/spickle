package com.github.rvanheest.spickle.parser.xml

import com.github.rvanheest.spickle.parser.{ Parser, ParserFailedException }
import shapeless.{ ::, HNil }

import scala.language.postfixOps
import scala.util.{ Failure, Success, Try }
import scala.xml.{ NamespaceBinding, Node }

object XmlParser {

  type XmlParser[A] = Parser[Seq[Node], A]

  private def nodeItem: XmlParser[Node] = {
    Parser(ns => ns
      .headOption
      .map(head => (Try(head), ns.tail))
      .getOrElse((Failure(ParserFailedException("can't parse an empty node sequence")), Seq.empty)))
  }

  def emptyNode(name: String): XmlParser[Unit] = {
    XmlParser.node(name).map(_ => ())
  }

  def node(name: String): XmlParser[Node] = {
    nodeItem.transform {
      case (head, tail) if head.label == name => (Success(head), tail)
      case (head, tail) => (Failure(ParserFailedException(s"could not find an element with name '$name'")), head +: tail)
    }
  }

  def stringNode(name: String): XmlParser[String] = {
    node(name).map(_.text)
  }

  def branchNode[A](name: String)(subParser: XmlParser[A]): XmlParser[A] = {
    Parser(node(name).map(_.child).parse(_) match {
      case (Success(childNodes), rest) =>
        val (r, rest2) = subParser.parse(childNodes)
        (r, rest2 ++ rest)
      case (Failure(e), rest) => (Failure(e), rest)
    })
  }

  private def attributeItem: XmlParser[Node] = {
    Parser(ns => ns
      .headOption
      .map(head => (Try(head), ns))
      .getOrElse((Failure(ParserFailedException("you're trying to parse an attribute in an empty xml Node")), Seq.empty)))
  }

  def attribute(attr: String): XmlParser[String] = {
    attributeItem
      .map(_ \@ attr)
      .satisfy(_.nonEmpty, _ => s"attribute '$attr' is not found or is empty")
  }

  def namespaceAttribute(attr: String)(implicit namespace: NamespaceBinding): XmlParser[String] = {
    // notice that _.attributes(...) can be null!!!
    attributeItem
      .map(_.attributes(namespace.uri, namespace, attr))
      .satisfy(Option(_).isDefined, _ => s"attribute '$attr' with namespace '$namespace' is not found")
      .satisfy(_.nonEmpty, _ => s"attribute '$attr' with namespace '$namespace' is empty")
      .map { case Seq(head, _ @ _*) => head.text }
  }

  def fromAllMandatory[T](parser: XmlParser[T]): AllParserBuilder[T :: HNil] = {
    AllParserBuilder.fromMandatory(parser)
  }

  def fromAllOptional[T](parser: XmlParser[T]): AllParserBuilder[Option[T] :: HNil] = {
    AllParserBuilder.fromOptional(parser)
  }
}
