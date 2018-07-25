package com.github.rvanheest.spickle.parser.xml

import com.github.rvanheest.spickle.parser.{ Parser, ParserFailedException }
import shapeless.{ ::, HNil }

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
      .map { case Seq(head, _ @ _*) => head.text }
  }

  def fromAllMandatory[T](parser: XmlParser[T]): AllParserBuilder[T :: HNil] = {
    AllParserBuilder.fromMandatory(parser)
  }

  def fromAllOptional[T](parser: XmlParser[T]): AllParserBuilder[Option[T] :: HNil] = {
    AllParserBuilder.fromOptional(parser)
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

  // TODO remove all parsers
  def all[T1, S1](p1: XmlParser[T1])(f1: Option[T1] => S1): XmlParser[S1] = {
    allPostProcess {
      allWorker(p1)(f1)
    }
  }

  def all[T1, S1, T2, S2](p1: XmlParser[T1],
                          p2: XmlParser[T2],
                         )
                         (f1: Option[T1] => S1,
                          f2: Option[T2] => S2,
                         ): XmlParser[(S1, S2)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
      } yield (res1, res2)
    }
  }

  def all[T1, S1, T2, S2, T3, S3](p1: XmlParser[T1],
                                  p2: XmlParser[T2],
                                  p3: XmlParser[T3],
                                 )
                                 (f1: Option[T1] => S1,
                                  f2: Option[T2] => S2,
                                  f3: Option[T3] => S3,
                                 ): XmlParser[(S1, S2, S3)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
        res3 <- allWorker(p3)(f3)
      } yield (res1, res2, res3)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4](p1: XmlParser[T1],
                                          p2: XmlParser[T2],
                                          p3: XmlParser[T3],
                                          p4: XmlParser[T4],
                                         )
                                         (f1: Option[T1] => S1,
                                          f2: Option[T2] => S2,
                                          f3: Option[T3] => S3,
                                          f4: Option[T4] => S4,
                                         ): XmlParser[(S1, S2, S3, S4)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
        res3 <- allWorker(p3)(f3)
        res4 <- allWorker(p4)(f4)
      } yield (res1, res2, res3, res4)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5](p1: XmlParser[T1],
                                                  p2: XmlParser[T2],
                                                  p3: XmlParser[T3],
                                                  p4: XmlParser[T4],
                                                  p5: XmlParser[T5],
                                                 )
                                                 (f1: Option[T1] => S1,
                                                  f2: Option[T2] => S2,
                                                  f3: Option[T3] => S3,
                                                  f4: Option[T4] => S4,
                                                  f5: Option[T5] => S5,
                                                 ): XmlParser[(S1, S2, S3, S4, S5)] = {
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

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6](p1: XmlParser[T1],
          p2: XmlParser[T2],
          p3: XmlParser[T3],
          p4: XmlParser[T4],
          p5: XmlParser[T5],
          p6: XmlParser[T6],
         )
         (f1: Option[T1] => S1,
          f2: Option[T2] => S2,
          f3: Option[T3] => S3,
          f4: Option[T4] => S4,
          f5: Option[T5] => S5,
          f6: Option[T6] => S6,
         ): XmlParser[(S1, S2, S3, S4, S5, S6)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
        res3 <- allWorker(p3)(f3)
        res4 <- allWorker(p4)(f4)
        res5 <- allWorker(p5)(f5)
        res6 <- allWorker(p6)(f6)
      } yield (res1, res2, res3, res4, res5, res6)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7](p1: XmlParser[T1],
                  p2: XmlParser[T2],
                  p3: XmlParser[T3],
                  p4: XmlParser[T4],
                  p5: XmlParser[T5],
                  p6: XmlParser[T6],
                  p7: XmlParser[T7],
                 )
                 (f1: Option[T1] => S1,
                  f2: Option[T2] => S2,
                  f3: Option[T3] => S3,
                  f4: Option[T4] => S4,
                  f5: Option[T5] => S5,
                  f6: Option[T6] => S6,
                  f7: Option[T7] => S7,
                 ): XmlParser[(S1, S2, S3, S4, S5, S6, S7)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
        res3 <- allWorker(p3)(f3)
        res4 <- allWorker(p4)(f4)
        res5 <- allWorker(p5)(f5)
        res6 <- allWorker(p6)(f6)
        res7 <- allWorker(p7)(f7)
      } yield (res1, res2, res3, res4, res5, res6, res7)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8](p1: XmlParser[T1],
                          p2: XmlParser[T2],
                          p3: XmlParser[T3],
                          p4: XmlParser[T4],
                          p5: XmlParser[T5],
                          p6: XmlParser[T6],
                          p7: XmlParser[T7],
                          p8: XmlParser[T8],
                         )
                         (f1: Option[T1] => S1,
                          f2: Option[T2] => S2,
                          f3: Option[T3] => S3,
                          f4: Option[T4] => S4,
                          f5: Option[T5] => S5,
                          f6: Option[T6] => S6,
                          f7: Option[T7] => S7,
                          f8: Option[T8] => S8,
                         ): XmlParser[(S1, S2, S3, S4, S5, S6, S7, S8)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
        res3 <- allWorker(p3)(f3)
        res4 <- allWorker(p4)(f4)
        res5 <- allWorker(p5)(f5)
        res6 <- allWorker(p6)(f6)
        res7 <- allWorker(p7)(f7)
        res8 <- allWorker(p8)(f8)
      } yield (res1, res2, res3, res4, res5, res6, res7, res8)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8, T9, S9](p1: XmlParser[T1],
                                  p2: XmlParser[T2],
                                  p3: XmlParser[T3],
                                  p4: XmlParser[T4],
                                  p5: XmlParser[T5],
                                  p6: XmlParser[T6],
                                  p7: XmlParser[T7],
                                  p8: XmlParser[T8],
                                  p9: XmlParser[T9],
                                 )
                                 (f1: Option[T1] => S1,
                                  f2: Option[T2] => S2,
                                  f3: Option[T3] => S3,
                                  f4: Option[T4] => S4,
                                  f5: Option[T5] => S5,
                                  f6: Option[T6] => S6,
                                  f7: Option[T7] => S7,
                                  f8: Option[T8] => S8,
                                  f9: Option[T9] => S9,
                                 ): XmlParser[(S1, S2, S3, S4, S5, S6, S7, S8, S9)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
        res3 <- allWorker(p3)(f3)
        res4 <- allWorker(p4)(f4)
        res5 <- allWorker(p5)(f5)
        res6 <- allWorker(p6)(f6)
        res7 <- allWorker(p7)(f7)
        res8 <- allWorker(p8)(f8)
        res9 <- allWorker(p9)(f9)
      } yield (res1, res2, res3, res4, res5, res6, res7, res8, res9)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8, T9, S9, T10, S10](p1: XmlParser[T1],
                                            p2: XmlParser[T2],
                                            p3: XmlParser[T3],
                                            p4: XmlParser[T4],
                                            p5: XmlParser[T5],
                                            p6: XmlParser[T6],
                                            p7: XmlParser[T7],
                                            p8: XmlParser[T8],
                                            p9: XmlParser[T9],
                                            p10: XmlParser[T10],
                                           )
                                           (f1: Option[T1] => S1,
                                            f2: Option[T2] => S2,
                                            f3: Option[T3] => S3,
                                            f4: Option[T4] => S4,
                                            f5: Option[T5] => S5,
                                            f6: Option[T6] => S6,
                                            f7: Option[T7] => S7,
                                            f8: Option[T8] => S8,
                                            f9: Option[T9] => S9,
                                            f10: Option[T10] => S10,
                                           ): XmlParser[(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
        res3 <- allWorker(p3)(f3)
        res4 <- allWorker(p4)(f4)
        res5 <- allWorker(p5)(f5)
        res6 <- allWorker(p6)(f6)
        res7 <- allWorker(p7)(f7)
        res8 <- allWorker(p8)(f8)
        res9 <- allWorker(p9)(f9)
        res10 <- allWorker(p10)(f10)
      } yield (res1, res2, res3, res4, res5, res6, res7, res8, res9, res10)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8, T9, S9, T10, S10,
  T11, S11](p1: XmlParser[T1],
            p2: XmlParser[T2],
            p3: XmlParser[T3],
            p4: XmlParser[T4],
            p5: XmlParser[T5],
            p6: XmlParser[T6],
            p7: XmlParser[T7],
            p8: XmlParser[T8],
            p9: XmlParser[T9],
            p10: XmlParser[T10],
            p11: XmlParser[T11],
           )
           (f1: Option[T1] => S1,
            f2: Option[T2] => S2,
            f3: Option[T3] => S3,
            f4: Option[T4] => S4,
            f5: Option[T5] => S5,
            f6: Option[T6] => S6,
            f7: Option[T7] => S7,
            f8: Option[T8] => S8,
            f9: Option[T9] => S9,
            f10: Option[T10] => S10,
            f11: Option[T11] => S11,
           ): XmlParser[(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
        res3 <- allWorker(p3)(f3)
        res4 <- allWorker(p4)(f4)
        res5 <- allWorker(p5)(f5)
        res6 <- allWorker(p6)(f6)
        res7 <- allWorker(p7)(f7)
        res8 <- allWorker(p8)(f8)
        res9 <- allWorker(p9)(f9)
        res10 <- allWorker(p10)(f10)
        res11 <- allWorker(p11)(f11)
      } yield (res1, res2, res3, res4, res5, res6, res7, res8, res9, res10, res11)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8, T9, S9, T10, S10,
  T11, S11, T12, S12](p1: XmlParser[T1],
                      p2: XmlParser[T2],
                      p3: XmlParser[T3],
                      p4: XmlParser[T4],
                      p5: XmlParser[T5],
                      p6: XmlParser[T6],
                      p7: XmlParser[T7],
                      p8: XmlParser[T8],
                      p9: XmlParser[T9],
                      p10: XmlParser[T10],
                      p11: XmlParser[T11],
                      p12: XmlParser[T12],
                     )
                     (f1: Option[T1] => S1,
                      f2: Option[T2] => S2,
                      f3: Option[T3] => S3,
                      f4: Option[T4] => S4,
                      f5: Option[T5] => S5,
                      f6: Option[T6] => S6,
                      f7: Option[T7] => S7,
                      f8: Option[T8] => S8,
                      f9: Option[T9] => S9,
                      f10: Option[T10] => S10,
                      f11: Option[T11] => S11,
                      f12: Option[T12] => S12,
                     ): XmlParser[(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12)] = {
    allPostProcess {
      for {
        res1 <- allWorker(p1)(f1)
        res2 <- allWorker(p2)(f2)
        res3 <- allWorker(p3)(f3)
        res4 <- allWorker(p4)(f4)
        res5 <- allWorker(p5)(f5)
        res6 <- allWorker(p6)(f6)
        res7 <- allWorker(p7)(f7)
        res8 <- allWorker(p8)(f8)
        res9 <- allWorker(p9)(f9)
        res10 <- allWorker(p10)(f10)
        res11 <- allWorker(p11)(f11)
        res12 <- allWorker(p12)(f12)
      } yield (res1, res2, res3, res4, res5, res6, res7, res8, res9, res10, res11, res12)
    }
  }

  def optional[X]: Option[X] => Option[X] = identity

  def mandatory[X]: Option[X] => X = {
    case Some(x) => x
    case None => throw ParserFailedException("missing mandatory element in any")
  }
}
