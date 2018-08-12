package com.github.rvanheest.spickle.pickle.xml

import com.github.rvanheest.spickle.parser.xml.XmlParser
import com.github.rvanheest.spickle.pickle.Pickle
import com.github.rvanheest.spickle.serializer.xml.XmlSerializer
import shapeless.{ ::, HNil }

import scala.language.reflectiveCalls
import scala.util.Try
import scala.xml.{ NamespaceBinding, Node, NodeSeq }

object XmlPickle {

  type XmlPickle[A] = Pickle[Seq[Node], A]

  implicit class XmlPickleExtension[A](val pickle: XmlPickle[A]) extends AnyVal {
    def serialize(input: A): Try[Seq[Node]] = pickle.serialize(input, NodeSeq.Empty)
  }

  def emptyNode(name: String): XmlPickle[Unit] = {
    Pickle(
      serializer = XmlSerializer.emptyNode(name),
      parser = XmlParser.emptyNode(name)
    )
  }

  def node(name: String): XmlPickle[Node] = {
    Pickle(
      serializer = XmlSerializer.node(name),
      parser = XmlParser.node(name)
    )
  }

  def stringNode(name: String): XmlPickle[String] = {
    Pickle(
      serializer = XmlSerializer.stringNode(name),
      parser = XmlParser.stringNode(name)
    )
  }

  def branchNode[A](name: String)(pickleA: XmlPickle[A]): XmlPickle[A] = {
    Pickle(
      serializer = XmlSerializer.branchNode(name)(pickleA.serializer),
      parser = XmlParser.branchNode(name)(pickleA.parser)
    )
  }

  def attribute(name: String): XmlPickle[String] = {
    Pickle(
      serializer = XmlSerializer.attribute(name),
      parser = XmlParser.attribute(name)
    )
  }

  def attribute(name: String, namespace: NamespaceBinding): XmlPickle[String] = {
    Pickle(
      serializer = XmlSerializer.attribute(name, namespace),
      parser = XmlParser.attribute(name, namespace)
    )
  }

  def fromAllMandatory[T](parser: XmlPickle[T]): AllPickleBuilder[T :: HNil] = {
    AllPickleBuilder.fromMandatory(parser)
  }

  def fromAllOptional[T](parser: XmlPickle[T]): AllPickleBuilder[Option[T] :: HNil] = {
    AllPickleBuilder.fromOptional(parser)
  }

  def collect[T](pickle: XmlPickle[T]): XmlPickle[Seq[T]] = {
    Pickle(
      serializer = XmlSerializer.collect(pickle.serializer),
      parser = XmlParser.collect(pickle.parser)
    )
  }
}
