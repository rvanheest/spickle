package com.github.rvanheest.spickle.example.xml.baseball

import com.github.rvanheest.spickle.pickle.xml.XmlPickle._

import scala.util.Success
import scala.xml.PrettyPrinter

object BaseBallPickleExample extends App with BaseBall with BaseBallXml with BaseBallPickle {

  val (Success(season), rest) = pickleSeason.parse(xml)
  val Success(seasonXml) = pickleSeason.serialize(season)

  println(season)
  println(rest)
  for (xml <- seasonXml) {
    println(new PrettyPrinter(160, 4).format(xml))
  }
}
