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

  def node(name: String): XmlParser[Node] = {
    node(name, None)
  }

  def node(name: String, namespace: NamespaceBinding): XmlParser[Node] = {
    node(name, Some(namespace))
  }

  private def node(name: String, namespace: Option[NamespaceBinding]): XmlParser[Node] = {
    nodeItem.transform {
      case (head, tail) if head.label == name && namespace.forall(ns => head.prefix == ns.prefix) =>
        (Success(head), tail)
      case (head, tail) =>
        val msg = namespace
          .map(ns => s"could not find an element with name '$name' and namespace '${ ns.toString.trim }'")
          .getOrElse(s"could not find an element with name '$name'")
        (Failure(ParserFailedException(msg)), head +: tail)
    }
  }

  def emptyNode(name: String): XmlParser[Unit] = {
    emptyNode(name, None)
  }

  def emptyNode(name: String, namespace: NamespaceBinding): XmlParser[Unit] = {
    emptyNode(name, Some(namespace))
  }

  private def emptyNode(name: String, namespace: Option[NamespaceBinding]): XmlParser[Unit] = {
    node(name, namespace).map(_ => ())
  }

  def stringNode(name: String): XmlParser[String] = {
    stringNode(name, None)
  }

  def stringNode(name: String, namespace: NamespaceBinding): XmlParser[String] = {
    stringNode(name, Some(namespace))
  }

  private def stringNode(name: String, namespace: Option[NamespaceBinding]): XmlParser[String] = {
    node(name, namespace).map(_.text)
  }

  def branchNode[A](name: String)(subParser: XmlParser[A]): XmlParser[A] = {
    branchNode(name, None)(subParser)
  }

  def branchNode[A](name: String, namespace: NamespaceBinding)(subParser: XmlParser[A]): XmlParser[A] = {
    branchNode(name, Some(namespace))(subParser)
  }

  private def branchNode[A](name: String, namespace: Option[NamespaceBinding])(subParser: XmlParser[A]): XmlParser[A] = {
    Parser(node(name, namespace).map(_.child).parse(_) match {
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
