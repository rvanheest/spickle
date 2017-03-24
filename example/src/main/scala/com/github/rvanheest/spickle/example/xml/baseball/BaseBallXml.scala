package com.github.rvanheest.spickle.example.xml.baseball

import scala.xml.{ Elem, Node, Utility }

trait BaseBallXml {

  // @formatter:off
  private val season: Elem = <SEASON YEAR="1998">
    <LEAGUE NAME="National League">
      <DIVISION NAME="East">
        <TEAM CITY="Atlanta" NAME="Braves">
          <PLAYER GIVEN_NAME="Marty" SURNAME="Malloy" POSITION="Second Base" GAMES="11" GAMES_STARTED="8" AT_BATS="28" RUNS="3" HITS="5" DOUBLES="1" TRIPLES="0" HOME_RUNS="1" RBI="1" STEALS="0" CAUGHT_STEALING="0" SACRIFICE_HITS="0" SACRIFICE_FLIES="0" ERRORS="0" WALKS="2" STRUCK_OUT="2" HIT_BY_PITCH="0"/>
          <PLAYER GIVEN_NAME="Ozzie" SURNAME="Guillen" POSITION="Shortstop" GAMES="83" GAMES_STARTED="59" AT_BATS="264" RUNS="35" HITS="73" DOUBLES="15" TRIPLES="1" HOME_RUNS="1" RBI="22" STEALS="1" CAUGHT_STEALING="4" SACRIFICE_HITS="4" SACRIFICE_FLIES="2" ERRORS="6" WALKS="24" STRUCK_OUT="25" HIT_BY_PITCH="1"/>
          <PLAYER GIVEN_NAME="Danny" SURNAME="Bautista" POSITION="Outfield" GAMES="82" GAMES_STARTED="27" AT_BATS="144" RUNS="17" HITS="36" DOUBLES="11" TRIPLES="0" HOME_RUNS="3" RBI="17" STEALS="1" CAUGHT_STEALING="0" SACRIFICE_HITS="3" SACRIFICE_FLIES="2" ERRORS="2" WALKS="7" STRUCK_OUT="21" HIT_BY_PITCH="0"/>
          <PLAYER GIVEN_NAME="Gerald" SURNAME="Williams" POSITION="Outfield" GAMES="129" GAMES_STARTED="51" AT_BATS="266" RUNS="46" HITS="81" DOUBLES="18" TRIPLES="3" HOME_RUNS="10" RBI="44" STEALS="11" CAUGHT_STEALING="5" SACRIFICE_HITS="2" SACRIFICE_FLIES="1" ERRORS="5" WALKS="17" STRUCK_OUT="48" HIT_BY_PITCH="3"/>
          <PLAYER GIVEN_NAME="Tom" SURNAME="Glavine" POSITION="Starting Pitcher" GAMES="33" GAMES_STARTED="33" WINS="20" LOSSES="6" SAVES="0" COMPLETE_GAMES="4" SHUT_OUTS="3" ERA="2.47" INNINGS="229.1" HOME_RUNS_AGAINST="13" RUNS_AGAINST="67" EARNED_RUNS="63" HIT_BATTER="2" WILD_PITCHES="3" BALK="0" WALKED_BATTER="74" STRUCK_OUT_BATTER="157"/>
          <PLAYER GIVEN_NAME="Javier" SURNAME="Lopez" POSITION="Catcher" GAMES="133" GAMES_STARTED="124" AT_BATS="489" RUNS="73" HITS="139" DOUBLES="21" TRIPLES="1" HOME_RUNS="34" RBI="106" STEALS="5" CAUGHT_STEALING="3" SACRIFICE_HITS="1" SACRIFICE_FLIES="8" ERRORS="5" WALKS="30" STRUCK_OUT="85" HIT_BY_PITCH="6"/>
          <PLAYER GIVEN_NAME="Ryan" SURNAME="Klesko" POSITION="Outfield" GAMES="129" GAMES_STARTED="124" AT_BATS="427" RUNS="69" HITS="117" DOUBLES="29" TRIPLES="1" HOME_RUNS="18" RBI="70" STEALS="5" CAUGHT_STEALING="3" SACRIFICE_HITS="0" SACRIFICE_FLIES="4" ERRORS="2" WALKS="56" STRUCK_OUT="66" HIT_BY_PITCH="3"/>
          <PLAYER GIVEN_NAME="Andres" SURNAME="Galarraga" POSITION="First Base" GAMES="153" GAMES_STARTED="151" AT_BATS="555" RUNS="103" HITS="169" DOUBLES="27" TRIPLES="1" HOME_RUNS="44" RBI="121" STEALS="7" CAUGHT_STEALING="6" SACRIFICE_HITS="0" SACRIFICE_FLIES="5" ERRORS="11" WALKS="63" STRUCK_OUT="146" HIT_BY_PITCH="25"/>
          <PLAYER GIVEN_NAME="Wes" SURNAME="Helms" POSITION="Third Base" GAMES="7" GAMES_STARTED="2" AT_BATS="13" RUNS="2" HITS="4" DOUBLES="1" TRIPLES="0" HOME_RUNS="1" RBI="2" STEALS="0" CAUGHT_STEALING="0" SACRIFICE_HITS="0" SACRIFICE_FLIES="0" ERRORS="1" WALKS="0" STRUCK_OUT="4" HIT_BY_PITCH="0"/>
        </TEAM>
        <TEAM CITY="Florida" NAME="Marlins"/>
        <TEAM CITY="Montreal" NAME="Expos"/>
        <TEAM CITY="New York" NAME="Mets"/>
        <TEAM CITY="Philadelphia" NAME="Phillies"/>
      </DIVISION>
      <DIVISION NAME="Central">
        <TEAM CITY="Chicago" NAME="Cubs"/>
        <TEAM CITY="Cincinnati" NAME="Reds"/>
        <TEAM CITY="Houston" NAME="Astros"/>
        <TEAM CITY="Milwaukee" NAME="Brewers"/>
        <TEAM CITY="Pittsburgh" NAME="Pirates"/>
        <TEAM CITY="St. Louis" NAME="Cardinals"/>
      </DIVISION>
      <DIVISION NAME="West">
        <TEAM CITY="Arizona" NAME="Diamondbacks"/>
        <TEAM CITY="Colorado" NAME="Rockies"/>
        <TEAM CITY="Los Angeles" NAME="Dodgers"/>
        <TEAM CITY="San Diego" NAME="Padres"/>
        <TEAM CITY="San Francisco" NAME="Giants"/>
      </DIVISION>
    </LEAGUE>
    <LEAGUE NAME="American League">
      <DIVISION NAME="East">
        <TEAM CITY="Baltimore" NAME="Orioles"/>
        <TEAM CITY="Boston" NAME="Red Sox"/>
        <TEAM CITY="New York" NAME="Yankees"/>
        <TEAM CITY="Tampa Bay" NAME="Devil Rays"/>
        <TEAM CITY="Toronto" NAME="Blue Jays"/>
      </DIVISION>
      <DIVISION NAME="Central">
        <TEAM CITY="Chicago" NAME="White Sox"/>
        <TEAM CITY="Kansas City" NAME="Royals"/>
        <TEAM CITY="Detroit" NAME="Tigers"/>
        <TEAM CITY="Cleveland" NAME="Indians"/>
        <TEAM CITY="Minnesota" NAME="Twins"/>
      </DIVISION>
      <DIVISION NAME="West">
        <TEAM CITY="Anaheim" NAME="Angels"/>
        <TEAM CITY="Oakland" NAME="Athletics"/>
        <TEAM CITY="Seattle" NAME="Mariners"/>
        <TEAM CITY="Texas" NAME="Rangers"/>
      </DIVISION>
    </LEAGUE>
  </SEASON>
  // @formatter:on

  val xml: Node = Utility.trim(season)
}
