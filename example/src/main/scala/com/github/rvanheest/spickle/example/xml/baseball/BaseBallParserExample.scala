package com.github.rvanheest.spickle.example.xml.baseball

object BaseBallParserExample extends App with BaseBall with BaseBallXml with BaseBallParser {

  println(parseSeason.parse(xml))
}
