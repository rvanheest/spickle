package com.github.rvanheest.spickle.pickle.xml

import com.github.rvanheest.spickle.parser.xml.XmlParser
import com.github.rvanheest.spickle.pickle.Pickle
import com.github.rvanheest.spickle.serializer.xml.XmlSerializer

import scala.language.reflectiveCalls
import scala.xml._

object XmlPickle {

  type XmlPickle[A] = Pickle[Seq[Node], A]

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

  def namespaceAttribute(name: String)(implicit namespace: NamespaceBinding): XmlPickle[String] = {
    Pickle(
      serializer = XmlSerializer.namespaceAttribute(name),
      parser = XmlParser.namespaceAttribute(name)
    )
  }

  case class ISO[X, XS](run: X => XS, undo: XS => X)

  def all[T, TS](p1: XmlPickle[T])(f1: ISO[Option[T], TS]): XmlPickle[TS] = {
    Pickle(
      serializer = XmlSerializer.all(p1.serializer)(f1.undo),
      parser = XmlParser.all(p1.parser)(f1.run)
    )
  }

  def all[T, TS, R, RS](p1: XmlPickle[T],
                        p2: XmlPickle[R])
                       (f1: ISO[Option[T], TS],
                        f2: ISO[Option[R], RS]): XmlPickle[(TS, RS)] = {
    Pickle(
      serializer = XmlSerializer.all(p1.serializer, p2.serializer)(f1.undo, f2.undo),
      parser = XmlParser.all(p1.parser, p2.parser)(f1.run, f2.run)
    )
  }

  def all[T, TS, R, RS, S, SS](p1: XmlPickle[T],
                               p2: XmlPickle[R],
                               p3: XmlPickle[S])
                              (f1: ISO[Option[T], TS],
                               f2: ISO[Option[R], RS],
                               f3: ISO[Option[S], SS]): XmlPickle[(TS, RS, SS)] = {
    Pickle(
      serializer = XmlSerializer.all(p1.serializer, p2.serializer, p3.serializer)(f1.undo, f2.undo, f3.undo),
      parser = XmlParser.all(p1.parser, p2.parser, p3.parser)(f1.run, f2.run, f3.run)
    )
  }

  def all[T, TS, R, RS, S, SS, V, VS](p1: XmlPickle[T],
                                      p2: XmlPickle[R],
                                      p3: XmlPickle[S],
                                      p4: XmlPickle[V])
                                     (f1: ISO[Option[T], TS],
                                      f2: ISO[Option[R], RS],
                                      f3: ISO[Option[S], SS],
                                      f4: ISO[Option[V], VS]): XmlPickle[(TS, RS, SS, VS)] = {
    Pickle(
      serializer = XmlSerializer.all(p1.serializer, p2.serializer, p3.serializer, p4.serializer)(f1.undo, f2.undo, f3.undo, f4.undo),
      parser = XmlParser.all(p1.parser, p2.parser, p3.parser, p4.parser)(f1.run, f2.run, f3.run, f4.run)
    )
  }

  def all[T, TS, R, RS, S, SS, V, VS, W, WS](p1: XmlPickle[T],
                                             p2: XmlPickle[R],
                                             p3: XmlPickle[S],
                                             p4: XmlPickle[V],
                                             p5: XmlPickle[W])
                                            (f1: ISO[Option[T], TS],
                                             f2: ISO[Option[R], RS],
                                             f3: ISO[Option[S], SS],
                                             f4: ISO[Option[V], VS],
                                             f5: ISO[Option[W], WS]): XmlPickle[(TS, RS, SS, VS, WS)] = {
    Pickle(
      serializer = XmlSerializer.all(p1.serializer, p2.serializer, p3.serializer, p4.serializer, p5.serializer)(f1.undo, f2.undo, f3.undo, f4.undo, f5.undo),
      parser = XmlParser.all(p1.parser, p2.parser, p3.parser, p4.parser, p5.parser)(f1.run, f2.run, f3.run, f4.run, f5.run)
    )
  }

  def optional[X]: ISO[Option[X], Option[X]] = ISO(XmlParser.optional, XmlSerializer.optional)

  def mandatory[X]: ISO[Option[X], X] = ISO(XmlParser.mandatory, XmlSerializer.mandatory)
}
