package com.github.rvanheest.spickle.example.xml.baseball

trait BaseBall {

  case class Player(firstName: String,
                    lastName: String,
                    position: String,
                    atBats: Option[Int],
                    hits: Option[Int],
                    era: Option[Float])
  case class Team(name: String, city: String, players: Seq[Player])
  case class Division(name: String, teams: Seq[Team])
  case class League(name: String, divisions: Seq[Division])
  case class Season(year: Int, leagues: Seq[League])
}
