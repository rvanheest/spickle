package com.github.rvanheest.spickle.serializer.xml

import com.github.rvanheest.spickle.serializer.{ Serializer, SerializerFailedException }
import shapeless.{ ::, HNil }

import scala.util.{ Failure, Success, Try }
import scala.xml._

object XmlSerializer {

  type XmlSerializer[A] = Serializer[Seq[Node], A]

  implicit class XmlSerializerExtension[A](val serializer: XmlSerializer[A]) extends AnyVal {
    def serialize(input: A): Try[Seq[Node]] = serializer.serialize(input, NodeSeq.Empty)
  }

  def emptyNode(name: String): XmlSerializer[Unit] = {
    node(name).contramap(_ => <xml/>.copy(label = name))
  }

  def emptyNode(name: String, namespace: NamespaceBinding): XmlSerializer[Unit] = {
    node(name, namespace).contramap(_ => <xml/>.copy(label = name, scope = namespace))
  }

  def node(name: String): XmlSerializer[Node] = {
    node(name, None)
  }

  def node(name: String, namespace: NamespaceBinding): XmlSerializer[Node] = {
    node(name, Some(namespace))
  }

  private def node(name: String, namespace: Option[NamespaceBinding]): XmlSerializer[Node] = {
    Serializer {
      case (head, tail) if head.label == name && namespace.forall(_.uri == head.namespace) =>
        Success(head ++ tail)
      case (head, _) =>
        Failure(new NoSuchElementException(s"element '$head' does not contain an element with name '$name'"))
    }
  }

  def stringNode(name: String): XmlSerializer[String] = {
    node(name).contramap(s => <xml>{s}</xml>.copy(label = name))
  }

  def stringNode(name: String, namespace: NamespaceBinding): XmlSerializer[String] = {
    node(name, namespace).contramap(s => <xml>{s}</xml>.copy(label = name, scope = namespace))
  }

  def branchNode[A](name: String)(serializerA: XmlSerializer[A]): XmlSerializer[A] = {
    Serializer((a, xml) => serializerA.serialize(a, NodeSeq.Empty).map(nodes => <xml>{nodes}</xml>.copy(label = name) ++ xml))
  }

  def branchNode[A](name: String, namespace: NamespaceBinding)(serializerA: XmlSerializer[A]): XmlSerializer[A] = {
    Serializer((a, xml) => serializerA.serialize(a, NodeSeq.Empty).map(nodes => <xml>{nodes}</xml>.copy(label = name, scope = namespace) ++ xml))
  }

  def attribute(name: String): XmlSerializer[String] = {
    Serializer((s: String, xml: Seq[Node]) => Try {
      xml.headOption map {
        case elem: Elem => elem % new UnprefixedAttribute(name, s, Null) ++ xml.tail
        case x => throw SerializerFailedException(s"Can only add an attribute with name '$name' to elements: $x")
      } getOrElse {
        throw SerializerFailedException(s"Cannot add an attribute with name '$name' to an empty node sequence")
      }
    })
  }

  def attribute(name: String, namespace: NamespaceBinding): XmlSerializer[String] = {
    Serializer((s: String, xml: Seq[Node]) => Try {
      xml.headOption map {
        case elem: Elem => elem % new PrefixedAttribute(namespace.prefix, name, s, Null) ++ xml.tail
        case x => throw SerializerFailedException(s"Can only add an attribute with name '${ namespace.prefix }:$name' to elements: $x")
      } getOrElse {
        throw SerializerFailedException(s"Cannot add an attribute with name '${ namespace.prefix }:$name' to an empty node sequence")
      }
    })
  }

  def fromAllMandatory[T](serializer: XmlSerializer[T]): AllSerializerBuilder[T :: HNil] = {
    AllSerializerBuilder.fromMandatory(serializer)
  }

  def fromAllOptional[T](serializer: XmlSerializer[T]): AllSerializerBuilder[Option[T] :: HNil] = {
    AllSerializerBuilder.fromOptional(serializer)
  }

  def collect[T](serializer: XmlSerializer[T]): XmlSerializer[Seq[T]] = {
    serializer.many
  }
}
