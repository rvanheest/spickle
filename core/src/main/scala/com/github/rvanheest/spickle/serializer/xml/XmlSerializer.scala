package com.github.rvanheest.spickle.serializer.xml

import com.github.rvanheest.spickle.serializer.{ Serializer, SerializerFailedException }
import shapeless.{ ::, HNil }

import scala.util.{ Failure, Success, Try }
import scala.xml._

object XmlSerializer {

  type XmlSerializer[A] = Serializer[Seq[Node], A]

  def emptyNode(name: String): XmlSerializer[Unit] = {
    node(name).contramap[Unit](_ => <xml/>.copy(label = name))
  }

  def node(name: String): XmlSerializer[Node] = {
    Serializer {
      case (head, tail) if head.label == name => Success(head ++ tail)
      case (head, _) => Failure(new NoSuchElementException(s"element '$head' does not contain an element with name '$name'"))
    }
  }

  def stringNode(name: String): XmlSerializer[String] = {
    node(name).contramap[String](s => <xml>{s}</xml>.copy(label = name))
  }

  def branchNode[A](name: String)(serializerA: XmlSerializer[A]): XmlSerializer[A] = {
    Serializer((a, xml) => serializerA.serialize(a, NodeSeq.Empty).map(nodes => <xml>{nodes}</xml>.copy(label = name) ++ xml))
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

  def namespaceAttribute(name: String)(implicit namespace: NamespaceBinding): XmlSerializer[String] = {
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
}
