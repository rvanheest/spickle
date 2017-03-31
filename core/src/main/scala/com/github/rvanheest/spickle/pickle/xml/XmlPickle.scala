package com.github.rvanheest.spickle.pickle.xml

import com.github.rvanheest.spickle.parser.xml.XmlParser
import com.github.rvanheest.spickle.pickle.Pickle

import scala.language.reflectiveCalls
import scala.util.{ Failure, Success, Try }
import scala.xml._

object XmlPickle {

  type XmlPickle[A] = Pickle[A, Seq[Node]]

  def emptyNode(name: String): XmlPickle[Unit] = {
    Pickle(
      pickle = (_: Unit, xml: Seq[Node]) => Try { <xml/>.copy(label = name) ++ xml },
      unpickle = XmlParser.node(name).map(_ => ()).parse)
  }

  def node(name: String): XmlPickle[Node] = {
    Pickle(
      pickle = {
        case (head, tail) if head.label == name => Success(head ++ tail)
        case (head, _) => Failure(new NoSuchElementException(s"element '$head' does not contain an element with name '$name'"))
      },
      unpickle = XmlParser.node(name).parse)
  }

  def stringNode(name: String): XmlPickle[String] = {
    Pickle(
      pickle = (s: String, xml: Seq[Node]) => Try { <xml>{s}</xml>.copy(label = name) ++ xml },
      unpickle = XmlParser.stringNode(name).parse)
  }

  def branchNode[A](name: String)(pickleA: XmlPickle[A]): XmlPickle[A] = {
    Pickle(
      pickle = (a: A, xml: Seq[Node]) => pickleA.pickle(a, Nil).map(nodes => <xml>{nodes}</xml>.copy(label = name) ++ xml),
      unpickle = XmlParser.branchNode(name)(pickleA.parse).parse)
  }

  def attribute(name: String): XmlPickle[String] = {
    Pickle(
      pickle = (s: String, xml: Seq[Node]) => Try {
        xml.headOption map {
          case elem: Elem => elem % new UnprefixedAttribute(name, s, Null) ++ xml.tail
          case _ => sys.error("Can only add attributes to elements!")
        } getOrElse sys.error("Cannot add attributes to an empty sequence")
      },
      unpickle = XmlParser.attribute(name).parse)
  }

  def namespaceAttribute(name: String)(implicit namespace: NamespaceBinding): XmlPickle[String] = {
    Pickle(
      pickle = (s: String, xml: Seq[Node]) => Try {
        xml.headOption map {
          case elem: Elem => elem % new PrefixedAttribute(namespace.prefix, name, s, Null) ++ xml.tail
          case _ => sys.error("Can only add attributes to elements!")
        } getOrElse sys.error("Cannot add attributes to an empty sequence")
      },
      unpickle = XmlParser.namespaceAttribute(name).parse)
  }
}
