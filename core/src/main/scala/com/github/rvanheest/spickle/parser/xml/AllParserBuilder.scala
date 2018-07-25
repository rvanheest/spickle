package com.github.rvanheest.spickle.parser.xml

import com.github.rvanheest.spickle.parser.xml.AllParserBuilder._
import com.github.rvanheest.spickle.parser.xml.XmlParser.XmlParser
import com.github.rvanheest.spickle.parser.{ Parser, ParserFailedException }
import shapeless.{ ::, HList, HNil }

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }
import scala.xml.Node

case class AllParserBuilder[MyHList <: HList] private(private val aggregate: XmlParser[MyHList]) {

  def andMandatory[T](parser: XmlParser[T]): AllParserBuilder[T :: MyHList] = {
    and(parser)(mandatory)
  }

  def andOptional[T](parser: XmlParser[T]): AllParserBuilder[Option[T] :: MyHList] = {
    and(parser)(optional)
  }

  private def and[T, S](parser: XmlParser[T])(f: Option[T] => S): AllParserBuilder[S :: MyHList] = {
    AllParserBuilder(aggregate.flatMap(myHList => allWorker(parser)(f(_) :: myHList)))
  }

  def build: XmlParser[MyHList] = {
    Parser(ns => {
      aggregate.parse(ns) match {
        case (result, Seq()) => (result, Seq())
        // TODO is this Failure correct?
        case (_, xs) => (Failure(ParserFailedException("remaining elements found in any")), xs)
      }
    })
  }
}

object AllParserBuilder {

  def fromMandatory[T](parser: XmlParser[T]): AllParserBuilder[T :: HNil] = {
    AllParserBuilder(allWorker(parser)(mandatory(_) :: HNil))
  }

  def fromOptional[T](parser: XmlParser[T]): AllParserBuilder[Option[T] :: HNil] = {
    AllParserBuilder(allWorker(parser)(optional(_) :: HNil))
  }

  private def optional[X]: Option[X] => Option[X] = identity

  private def mandatory[X]: Option[X] => X = {
    case Some(x) => x
    case None => throw ParserFailedException("missing mandatory element in any")
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
        case (Success(v), remainder) => (Try {
          f(v)
        }, remainder)
        case (Failure(e), remainder) => (Failure(e), remainder)
      }
    })
  }
}
