package com.github.rvanheest.spickle.example.xml.baseball

import com.github.rvanheest.spickle.pickle.xml.XmlPickle
import com.github.rvanheest.spickle.pickle.xml.XmlPickle._

trait BaseBallPickle {
  this: BaseBall =>

  def picklePlayer(name: String): XmlPickle[Player] = {
    for {
      first <- attribute("GIVEN_NAME").seq[Player](_.firstName)
      last <- attribute("SURNAME").seq[Player](_.lastName)
      position <- attribute("POSITION").seq[Player](_.position)
      atBat <- attribute("AT_BATS").toInt.maybe.seq[Player](_.atBats)
      hits <- attribute("HITS").toInt.maybe.seq[Player](_.hits)
      era <- attribute("ERA").toFloat.maybe.seq[Player](_.era)
      _ <- emptyNode(name).seq[Player](_ => ())
    } yield Player(first, last, position, atBat, hits, era)
  }

  def pickleTeam(name: String): XmlPickle[Team] = {
    for {
      n <- attribute("NAME").seq[Team](_.name)
      city <- attribute("CITY").seq[Team](_.city)
      team <- branchNode(name) {
        for {
          players <- picklePlayer("PLAYER").many.seq[Team](_.players)
        } yield Team(n, city, players)
      }.seqId
    } yield team
  }

  def pickleDivision(name: String): XmlPickle[Division] = {
    for {
      n <- attribute("NAME").seq[Division](_.name)
      division <- branchNode(name) {
        for {
          teams <- pickleTeam("TEAM").many.seq[Division](_.teams)
        } yield Division(n, teams)
      }.seqId
    } yield division
  }

  def pickleLeague(name: String): XmlPickle[League] = {
    for {
      n <- attribute("NAME").seq[League](_.name)
      league <- branchNode(name) {
        for {
          divisions <- pickleDivision("DIVISION").many.seq[League](_.divisions)
        } yield League(n, divisions)
      }.seqId
    } yield league
  }

  def pickleSeason: XmlPickle[Season] = {
    for {
      year <- attribute("YEAR").toInt.seq[Season](_.year)
      season <- branchNode("SEASON") {
        for {
          leagues <- pickleLeague("LEAGUE").many.seq[Season](_.leagues)
        } yield Season(year, leagues)
      }.seqId
    } yield season
  }
}
