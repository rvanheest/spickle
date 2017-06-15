package com.github.rvanheest.spickle.pickle.xml

import com.github.rvanheest.spickle.parser.ParserFailedException
import com.github.rvanheest.spickle.parser.xml.XmlParser
import com.github.rvanheest.spickle.pickle.{ Pickle, PickleFailedException }

import scala.language.reflectiveCalls
import scala.util.{ Failure, Success, Try }
import scala.xml._

object XmlPickle {

  type XmlPickle[A] = Pickle[Seq[Node], A]

  def emptyNode(name: String): XmlPickle[Unit] = {
    Pickle(
      pickler = (_: Unit, xml: Seq[Node]) => Try { <xml/>.copy(label = name) ++ xml },
      parser = XmlParser.node(name).map(_ => ()))
  }

  def node(name: String): XmlPickle[Node] = {
    Pickle(
      pickler = {
        case (head, tail) if head.label == name => Success(head ++ tail)
        case (head, _) => Failure(new NoSuchElementException(s"element '$head' does not contain an element with name '$name'"))
      },
      parser = XmlParser.node(name))
  }

  def stringNode(name: String): XmlPickle[String] = {
    Pickle(
      pickler = (s: String, xml: Seq[Node]) => Try { <xml>{s}</xml>.copy(label = name) ++ xml },
      parser = XmlParser.stringNode(name))
  }

  def branchNode[A](name: String)(pickleA: XmlPickle[A]): XmlPickle[A] = {
    Pickle(
      pickler = (a: A, xml: Seq[Node]) => pickleA.pickler(a, NodeSeq.Empty).map(nodes => <xml>{nodes}</xml>.copy(label = name) ++ xml),
      parser = XmlParser.branchNode(name)(pickleA.parser))
  }

  def attribute(name: String): XmlPickle[String] = {
    Pickle(
      pickler = (s: String, xml: Seq[Node]) => Try {
        xml.headOption map {
          case elem: Elem => elem % new UnprefixedAttribute(name, s, Null) ++ xml.tail
          case x => throw PickleFailedException(s"Can only add an attribute with name '$name' to elements: $x")
        } getOrElse (throw PickleFailedException(s"Cannot add an attribute with name '$name' to an empty node sequence"))
      },
      parser = XmlParser.attribute(name))
  }

  def namespaceAttribute(name: String)(implicit namespace: NamespaceBinding): XmlPickle[String] = {
    Pickle(
      pickler = (s: String, xml: Seq[Node]) => Try {
        xml.headOption map {
          case elem: Elem => elem % new PrefixedAttribute(namespace.prefix, name, s, Null) ++ xml.tail
          case x => throw PickleFailedException(s"Can only add an attribute with name '${namespace.prefix}:$name' to elements: $x")
        } getOrElse (throw PickleFailedException(s"Cannot add an attribute with name '${namespace.prefix}:$name' to an empty node sequence"))
      },
      parser = XmlParser.namespaceAttribute(name))
  }

  case class ISO[X, XS](run: X => XS, undo: XS => X)

  def all[T, TS](p1: XmlPickle[T])(f1: ISO[Option[T], TS]): XmlPickle[TS] = {
    Pickle(
      pickler = (ts, xml: Seq[Node]) => p1.maybe.pickle(f1.undo(ts), xml),
      parser = XmlParser.all(p1.parser)(f1.run))
  }

  def all[T, TS, R, RS](p1: XmlPickle[T],
                        p2: XmlPickle[R])(
                         f1: ISO[Option[T], TS],
                         f2: ISO[Option[R], RS]): XmlPickle[(TS, RS)] = {
    Pickle(
      pickler = {
        case ((ts, rs), xml: Seq[Node]) =>
          type GroupedOption = (Option[T], Option[R])
          (for {
            res1 <- p1.maybe.seq[GroupedOption](_._1)
            res2 <- p2.maybe.seq[GroupedOption](_._2)
          } yield (res1, res2))
            .pickle((f1.undo(ts), f2.undo(rs)), xml)
      },
      parser = XmlParser.all(p1.parser, p2.parser)(f1.run, f2.run))
  }

  def all[T, TS, R, RS, S, SS](p1: XmlPickle[T],
                               p2: XmlPickle[R],
                               p3: XmlPickle[S])(
                                f1: ISO[Option[T], TS],
                                f2: ISO[Option[R], RS],
                                f3: ISO[Option[S], SS]): XmlPickle[(TS, RS, SS)] = {
    Pickle(
      pickler = {
        case ((ts, rs, ss), xml: Seq[Node]) =>
          type GroupedOption = (Option[T], Option[R], Option[S])
          (for {
            res1 <- p1.maybe.seq[GroupedOption](_._1)
            res2 <- p2.maybe.seq[GroupedOption](_._2)
            res3 <- p3.maybe.seq[GroupedOption](_._3)
          } yield (res1, res2, res3))
            .pickle((f1.undo(ts), f2.undo(rs), f3.undo(ss)), xml)
      },
      parser = XmlParser.all(p1.parser, p2.parser, p3.parser)(f1.run, f2.run, f3.run))
  }

  def all[T, TS, R, RS, S, SS, V, VS](p1: XmlPickle[T],
                                      p2: XmlPickle[R],
                                      p3: XmlPickle[S],
                                      p4: XmlPickle[V])(
                                       f1: ISO[Option[T], TS],
                                       f2: ISO[Option[R], RS],
                                       f3: ISO[Option[S], SS],
                                       f4: ISO[Option[V], VS]): XmlPickle[(TS, RS, SS, VS)] = {
    Pickle(
      pickler = {
        case ((ts, rs, ss, vs), xml: Seq[Node]) =>
          type GroupedOption = (Option[T], Option[R], Option[S], Option[V])
          (for {
            res1 <- p1.maybe.seq[GroupedOption](_._1)
            res2 <- p2.maybe.seq[GroupedOption](_._2)
            res3 <- p3.maybe.seq[GroupedOption](_._3)
            res4 <- p4.maybe.seq[GroupedOption](_._4)
          } yield (res1, res2, res3, res4))
            .pickle((f1.undo(ts), f2.undo(rs), f3.undo(ss), f4.undo(vs)), xml)
      },
      parser = XmlParser.all(p1.parser, p2.parser, p3.parser, p4.parser)(f1.run, f2.run, f3.run, f4.run))
  }

  def all[T, TS, R, RS, S, SS, V, VS, W, WS](p1: XmlPickle[T],
                                             p2: XmlPickle[R],
                                             p3: XmlPickle[S],
                                             p4: XmlPickle[V],
                                             p5: XmlPickle[W])(
                                              f1: ISO[Option[T], TS],
                                              f2: ISO[Option[R], RS],
                                              f3: ISO[Option[S], SS],
                                              f4: ISO[Option[V], VS],
                                              f5: ISO[Option[W], WS]): XmlPickle[(TS, RS, SS, VS, WS)] = {
    Pickle(
      pickler = {
        case ((ts, rs, ss, vs, ws), xml: Seq[Node]) =>
          type GroupedOption = (Option[T], Option[R], Option[S], Option[V], Option[W])
          (for {
            res1 <- p1.maybe.seq[GroupedOption](_._1)
            res2 <- p2.maybe.seq[GroupedOption](_._2)
            res3 <- p3.maybe.seq[GroupedOption](_._3)
            res4 <- p4.maybe.seq[GroupedOption](_._4)
            res5 <- p5.maybe.seq[GroupedOption](_._5)
          } yield (res1, res2, res3, res4, res5))
            .pickle((f1.undo(ts), f2.undo(rs), f3.undo(ss), f4.undo(vs), f5.undo(ws)), xml)
      },
      parser = XmlParser.all(p1.parser, p2.parser, p3.parser, p4.parser, p5.parser)(f1.run, f2.run, f3.run, f4.run, f5.run))
  }

  def optional[X]: ISO[Option[X], Option[X]] = ISO(XmlParser.optional, identity)

  def mandatory[X]: ISO[Option[X], X] = ISO(XmlParser.mandatory, Option(_))
}
