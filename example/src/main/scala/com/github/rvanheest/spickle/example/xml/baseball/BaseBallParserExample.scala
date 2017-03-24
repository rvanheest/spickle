package com.github.rvanheest.spickle.example.xml.baseball

import scala.xml.Utility

object BaseBallParserExample extends App with BaseBall with BaseBallXml with BaseBallParser {

  println(parseSeason.run(Utility.trim(xml)))
}
