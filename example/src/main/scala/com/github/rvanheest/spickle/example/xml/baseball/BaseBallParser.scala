package com.github.rvanheest.spickle.example.xml.baseball

import com.github.rvanheest.spickle.parser.xml.XmlParser._

trait BaseBallParser {
  this: BaseBall =>

  def parsePlayer(name: String): XmlParser[Player] = {
    for {
      first <- attributeId("GIVEN_NAME")
      last <- attributeId("SURNAME")
      position <- attributeId("POSITION")
      atBat <- attribute("AT_BATS")(_.toInt).maybe
      hits <- attribute("HITS")(_.toInt).maybe
      era <- attribute("ERA")(_.toFloat).maybe
      _ <- nodeWithName(name)
    } yield Player(first, last, position, atBat, hits, era)
  }

  def parseTeam(name: String): XmlParser[Team] = {
    for {
      n <- attributeId("NAME")
      city <- attributeId("CITY")
      team <- branchNode(name) {
        for {
          players <- parsePlayer("PLAYER").many
        } yield Team(n, city, players)
      }
    } yield team
  }

  def parseDivision(name: String): XmlParser[(String, Seq[Team])] = {
    for {
      n <- attributeId("NAME")
      division <- branchNode(name) {
        for {
          teams <- parseTeam("TEAM").many
        } yield n -> teams
      }
    } yield division
  }

  def parseLeague(name: String): XmlParser[(String, Divisions)] = {
    for {
      n <- attributeId("NAME")
      league <- branchNode(name) {
        for {
          divisions <- parseDivision("DIVISION").many.map(_.toMap)
        } yield n -> divisions
      }
    } yield league
  }

  def parseSeason: XmlParser[Season] = {
    for {
      year <- attribute("YEAR")(_.toInt)
      season <- branchNode("SEASON") {
        for {
          leagues <- parseLeague("LEAGUE").many.map(_.toMap)
        } yield Season(year, leagues)
      }
    } yield season
  }
}
