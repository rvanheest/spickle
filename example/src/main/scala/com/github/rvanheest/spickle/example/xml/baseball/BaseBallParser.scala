package com.github.rvanheest.spickle.example.xml.baseball

import com.github.rvanheest.spickle.parser.xml.XmlParser._

trait BaseBallParser {
  this: BaseBall =>

  def parsePlayer(name: String): XmlParser[Player] = {
    for {
      first <- attribute("GIVEN_NAME")
      last <- attribute("SURNAME")
      position <- attribute("POSITION")
      atBat <- attribute("AT_BATS").toInt.maybe
      hits <- attribute("HITS").toInt.maybe
      era <- attribute("ERA").toFloat.maybe
      _ <- node(name)
    } yield Player(first, last, position, atBat, hits, era)
  }

  def parseTeam(name: String): XmlParser[Team] = {
    for {
      n <- attribute("NAME")
      city <- attribute("CITY")
      team <- branchNode(name) {
        for {
          players <- parsePlayer("PLAYER").many
        } yield Team(n, city, players)
      }
    } yield team
  }

  def parseDivision(name: String): XmlParser[Division] = {
    for {
      n <- attribute("NAME")
      division <- branchNode(name) {
        for {
          teams <- parseTeam("TEAM").many
        } yield Division(n, teams)
      }
    } yield division
  }

  def parseLeague(name: String): XmlParser[League] = {
    for {
      n <- attribute("NAME")
      league <- branchNode(name) {
        for {
          divisions <- parseDivision("DIVISION").many
        } yield League(n, divisions)
      }
    } yield league
  }

  def parseSeason: XmlParser[Season] = {
    for {
      year <- attribute("YEAR").toInt
      season <- branchNode("SEASON") {
        for {
          leagues <- parseLeague("LEAGUE").many
        } yield Season(year, leagues)
      }
    } yield season
  }
}
