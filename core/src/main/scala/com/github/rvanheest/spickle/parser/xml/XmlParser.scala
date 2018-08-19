package com.github.rvanheest.spickle.parser.xml

import com.github.rvanheest.spickle.parser.{ Parser, ParserFailedException }
import shapeless.{ ::, HNil }

import scala.collection.mutable
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
      case (Success(childNodes), rest) => (subParser.eval(childNodes), rest)
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

  def attribute(attr: String, namespace: NamespaceBinding): XmlParser[String] = {
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

  def collect[T](parser: XmlParser[T]): XmlParser[Seq[T]] = {
    Parser(xml => {
      val results = mutable.ListBuffer.newBuilder[T]
      val notApplicable = mutable.ListBuffer.newBuilder[Node]

      for (x <- xml) {
        parser.eval(x) match {
          case Success(result) => results += result
          case Failure(_) => notApplicable += x
        }
      }

      (Success(results.result()), notApplicable.result())
    })
  }
}
