package com.github.rvanheest.spickle.serializer.xml

import com.github.rvanheest.spickle.serializer.{ Serializer, SerializerFailedException }

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

  def all[T, TS](s1: XmlSerializer[T])(f1: TS => Option[T]): XmlSerializer[TS] = {
    Serializer((ts, xml) => s1.maybe.serialize(f1(ts), xml))
  }

  def all[T, TS, R, RS](p1: XmlSerializer[T],
                        p2: XmlSerializer[R])(
                         f1: TS => Option[T],
                         f2: RS => Option[R]): XmlSerializer[(TS, RS)] = {
    type GroupedOption = (Option[T], Option[R])
    Serializer {
      case ((ts, rs), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r) => r })
          .serialize((f1(ts), f2(rs)), xml)
    }
  }

  def all[T, TS, R, RS, S, SS](p1: XmlSerializer[T],
                               p2: XmlSerializer[R],
                               p3: XmlSerializer[S])(
                                f1: TS => Option[T],
                                f2: RS => Option[R],
                                f3: SS => Option[S]): XmlSerializer[(TS, RS, SS)] = {
    type GroupedOption = (Option[T], Option[R], Option[S])
    Serializer {
      case ((ts, rs, ss), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r, _) => r })
          .combine(p3.maybe.contramap[GroupedOption] { case (_, _, s) => s })
          .serialize((f1(ts), f2(rs), f3(ss)), xml)
    }
  }

  def all[T, TS, R, RS, S, SS, V, VS](p1: XmlSerializer[T],
                                      p2: XmlSerializer[R],
                                      p3: XmlSerializer[S],
                                      p4: XmlSerializer[V])(
                                       f1: TS => Option[T],
                                       f2: RS => Option[R],
                                       f3: SS => Option[S],
                                       f4: VS => Option[V]): XmlSerializer[(TS, RS, SS, VS)] = {
    type GroupedOption = (Option[T], Option[R], Option[S], Option[V])
    Serializer {
      case ((ts, rs, ss, vs), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _, _, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r, _, _) => r })
          .combine(p3.maybe.contramap[GroupedOption] { case (_, _, s, _) => s })
          .combine(p4.maybe.contramap[GroupedOption] { case (_, _, _, v) => v })
          .serialize((f1(ts), f2(rs), f3(ss), f4(vs)), xml)
    }
  }

  def all[T, TS, R, RS, S, SS, V, VS, W, WS](p1: XmlSerializer[T],
                                             p2: XmlSerializer[R],
                                             p3: XmlSerializer[S],
                                             p4: XmlSerializer[V],
                                             p5: XmlSerializer[W])(
                                              f1: TS => Option[T],
                                              f2: RS => Option[R],
                                              f3: SS => Option[S],
                                              f4: VS => Option[V],
                                              f5: WS => Option[W]): XmlSerializer[(TS, RS, SS, VS, WS)] = {
    type GroupedOption = (Option[T], Option[R], Option[S], Option[V], Option[W])
    Serializer {
      case ((ts, rs, ss, vs, ws), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _, _, _, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r, _, _, _) => r })
          .combine(p3.maybe.contramap[GroupedOption] { case (_, _, s, _, _) => s })
          .combine(p4.maybe.contramap[GroupedOption] { case (_, _, _, v, _) => v })
          .combine(p5.maybe.contramap[GroupedOption] { case (_, _, _, _, w) => w })
          .serialize((f1(ts), f2(rs), f3(ss), f4(vs), f5(ws)), xml)
    }
  }

  def optional[X]: Option[X] => Option[X] = identity

  def mandatory[X]: X => Option[X] = Option(_)
}
