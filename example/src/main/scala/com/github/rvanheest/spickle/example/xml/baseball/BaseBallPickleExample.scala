package com.github.rvanheest.spickle.example.xml.baseball

import scala.util.Success

object BaseBallPickleExample extends App with BaseBall with BaseBallXml with BaseBallPickle {

  val (Success(season), rest) = pickleSeason.unpickle(xml)
  val Success(seasonXml) = pickleSeason.pickle(season, Seq.empty)

  println(season)
  println(rest)
  println(seasonXml)
}
