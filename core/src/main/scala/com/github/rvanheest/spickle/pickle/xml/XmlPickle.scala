package com.github.rvanheest.spickle.pickle.xml

import com.github.rvanheest.spickle.parser.xml.XmlParser
import com.github.rvanheest.spickle.pickle.{ Pickle, PickleBuilder }

import scala.language.reflectiveCalls
import scala.util.Try
import scala.xml._

case class XmlPickle[A](override val pickle: (A, Seq[Node]) => Try[Seq[Node]],
												override val unpickle: Seq[Node] => (Try[A], Seq[Node]))
	extends Pickle[A, Seq[Node]](pickle, unpickle) {

	type Repr[X] = XmlPickle[X]

	protected[this] implicit def builder[X]: PickleBuilder[X, Seq[Node], XmlPickle[X]] = XmlPickle.xmlPickleBuilder

	def toByte(implicit ev: A =:= String, ev2: String =:= A): Repr[Byte] = {
		this.seq[Byte](_.toString).map(ev(_).toByte)
	}

	def toShort(implicit ev: A =:= String, ev2: String =:= A): Repr[Short] = {
		this.seq[Short](_.toString).map(ev(_).toShort)
	}

	def toInt(implicit ev: A =:= String, ev2: String =:= A): Repr[Int] = {
		this.seq[Int](_.toString).map(ev(_).toInt)
	}

	def toLong(implicit ev: A =:= String, ev2: String =:= A): Repr[Long] = {
		this.seq[Long](_.toString).map(ev(_).toLong)
	}

	def toFloat(implicit ev: A =:= String, ev2: String =:= A): Repr[Float] = {
		this.seq[Float](_.toString).map(ev(_).toFloat)
	}

	def toDouble(implicit ev: A =:= String, ev2: String =:= A): Repr[Double] = {
		this.seq[Double](_.toString).map(ev(_).toDouble)
	}
}

object XmlPickle {

	protected[XmlPickle] implicit def xmlPickleBuilder[X]: PickleBuilder[X, Seq[Node], XmlPickle[X]] = {
		new PickleBuilder[X, Seq[Node], XmlPickle[X]] {
			def apply(pickle: (X, Seq[Node]) => Try[Seq[Node]], unpickle: Seq[Node] => (Try[X], Seq[Node])): XmlPickle[X] = {
				XmlPickle(pickle, unpickle)
			}
		}
	}

	def emptyNode(name: String): XmlPickle[Unit] = {
		XmlPickle(
			pickle = (_: Unit, xml: Seq[Node]) => Try { <xml/>.copy(label = name) ++ xml },
			unpickle = XmlParser.node(name).map(_ => ()).parse)
	}

	def string(name: String): XmlPickle[String] = {
		XmlPickle(
			pickle = (s: String, xml: Seq[Node]) => Try { <xml>{s}</xml>.copy(label = name) ++ xml },
			unpickle = XmlParser.nodeToString(name).parse)
	}

  def branchNode[A](name: String)(pickleA: XmlPickle[A]): XmlPickle[A] = {
    XmlPickle(
      pickle = (a: A, xml: Seq[Node]) => pickleA.pickle(a, Nil).map(nodes => <xml>{nodes}</xml>.copy(label = name) ++ xml),
      unpickle = XmlParser.branchNode(name)(pickleA.parse).parse)
  }

	def attribute(name: String): XmlPickle[String] = {
		XmlPickle(
			pickle = (s: String, xml: Seq[Node]) => Try {
				xml.headOption map {
					case elem: Elem => elem % new UnprefixedAttribute(name, s, Null) ++ xml.tail
					case _ => sys.error("Can only add attributes to elements!")
				} getOrElse sys.error("Cannot add attributes to an empty sequence")
			},
			unpickle = XmlParser.attribute(name).parse)
	}

	def namespaceAttribute(name: String)(implicit namespace: NamespaceBinding): XmlPickle[String] = {
		XmlPickle(
			pickle = (s: String, xml: Seq[Node]) => Try {
				xml.headOption map {
					case elem: Elem => elem % new PrefixedAttribute(namespace.prefix, name, s, Null) ++ xml.tail
					case _ => sys.error("Can only add attributes to elements!")
				} getOrElse sys.error("Cannot add attributes to an empty sequence")
			},
			unpickle = XmlParser.namespaceAttribute(name).parse)
	}
}
