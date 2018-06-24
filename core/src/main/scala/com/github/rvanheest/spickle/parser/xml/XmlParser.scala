package com.github.rvanheest.spickle.parser.xml

import com.github.rvanheest.spickle.parser.{ Parser, ParserFailedException }

import scala.annotation.tailrec
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
      case (Success(childNodes), rest) =>
        val (r, rest2) = subParser.parse(childNodes)
        (r, rest2 ++ rest)
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

  def namespaceAttribute(attr: String)(implicit namespace: NamespaceBinding): XmlParser[String] = {
    // notice that _.attributes(...) can be null!!!
    attributeItem
      .map(_.attributes(namespace.uri, namespace, attr))
      .satisfy(Option(_).isDefined, _ => s"attribute '$attr' with namespace '$namespace' is not found")
      .satisfy(_.nonEmpty, _ => s"attribute '$attr' with namespace '$namespace' is empty")
      .map { case Seq(head, _@_*) => head.text }
  }

  private def allWorker[T, TS](p: XmlParser[T])(f: Option[T] => TS): XmlParser[TS] = {
    @tailrec
    def recursive(nodesToInspect: Seq[Node], visitedNodesToSkip: Seq[Node] = Seq.empty): (Try[Option[T]], Seq[Node]) = {
      nodesToInspect match {
        case Seq() => (Success(None), visitedNodesToSkip)
        case Seq(node, tail @ _*) =>
          val (result, remainder) = p.maybe.parse(node)
          result match {
            case t @ (Success(Some(_)) | Failure(_)) => (t, visitedNodesToSkip ++ remainder ++ tail)
            case Success(None) => recursive(tail, visitedNodesToSkip :+ node)
          }
      }
    }

    Parser(ns => {
      recursive(ns) match {
        case (Success(v), remainder) => (Try { f(v) }, remainder)
        case (Failure(e), remainder) => (Failure(e), remainder)
      }
    })
  }

  private def allPostProcess[X](parser: XmlParser[X]): XmlParser[X] = {
    Parser(ns => {
      val (result, remainder) = parser.parse(ns)
      remainder match {
        case Seq() => (result, remainder)
        case xs => (Failure(ParserFailedException("remaining elements found in any")), xs)
      }
    })
  }

  def all[T, TS](p1: XmlParser[T])(f1: Option[T] => TS): XmlParser[TS] = {
    allPostProcess {
      allWorker(p1)(f1)
    }
  }

  def all[T, TS, R, RS](p1: XmlParser[T],
                        p2: XmlParser[R])(
                         f1: Option[T] => TS,
                         f2: Option[R] => RS): XmlParser[(TS, RS)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
      } yield (res1, res2)
    }
  }

  def all[T, TS, R, RS, S, SS](p1: XmlParser[T],
                               p2: XmlParser[R],
                               p3: XmlParser[S])(
                                f1: Option[T] => TS,
                                f2: Option[R] => RS,
                                f3: Option[S] => SS): XmlParser[(TS, RS, SS)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
        res3 <- allWorker(p3)(f3)
      } yield (res1, res2, res3)
    }
  }

  def all[T, TS, R, RS, S, SS, V, VS](p1: XmlParser[T],
                                      p2: XmlParser[R],
                                      p3: XmlParser[S],
                                      p4: XmlParser[V])(
                                       f1: Option[T] => TS,
                                       f2: Option[R] => RS,
                                       f3: Option[S] => SS,
                                       f4: Option[V] => VS): XmlParser[(TS, RS, SS, VS)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
        res3 <- allWorker(p3)(f3)
        res4 <- allWorker(p4)(f4)
      } yield (res1, res2, res3, res4)
    }
  }

  def all[T, TS, R, RS, S, SS, V, VS, W, WS](p1: XmlParser[T],
                                             p2: XmlParser[R],
                                             p3: XmlParser[S],
                                             p4: XmlParser[V],
                                             p5: XmlParser[W])(
                                              f1: Option[T] => TS,
                                              f2: Option[R] => RS,
                                              f3: Option[S] => SS,
                                              f4: Option[V] => VS,
                                              f5: Option[W] => WS): XmlParser[(TS, RS, SS, VS, WS)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
        res3 <- allWorker(p3)(f3)
        res4 <- allWorker(p4)(f4)
        res5 <- allWorker(p5)(f5)
      } yield (res1, res2, res3, res4, res5)
    }
  }

  def optional[X]: Option[X] => Option[X] = identity

  def mandatory[X]: Option[X] => X = {
    case Some(x) => x
    case None => throw ParserFailedException("missing mandatory element in any")
  }
}
