package com.github.rvanheest.spickle.example.xml.namespace

import java.nio.file.Paths

import com.github.rvanheest.spickle.example.xml.namespace.Namespace.Note
import com.github.rvanheest.spickle.parser.xml.XmlParser.{ XmlParser, _ }

import scala.util.Success
import scala.xml.{ NamespaceBinding, TopScope, Utility, XML }

object NamespaceParserRunner extends App {

  val path = Paths.get(getClass.getResource("/namespace/note1.xml").toURI)
  val xml = Utility.trim(XML.loadFile(path.toFile))

  println(xml)

  val (Success(note), remainder) = NamespaceParser.parseNote.parse(xml)
  println(s"result: $note")
  println(s"remainder: ${ if (remainder.isEmpty) "<empty>" else remainder }")
}

object NamespaceParser {

  val dc: NamespaceBinding = NamespaceBinding("dc", "http://purl.org/dc/elements/1.1/", TopScope)
  val dcterms: NamespaceBinding = NamespaceBinding("dcterms", "http://purl.org/dc/terms/", TopScope)

  def parseNote: XmlParser[Note] = {
    branchNode("note") {
      for {
        dcTitles <- parseDcTitles
        dctermsTitles <- parseDctermsTitles
        dctermsDescriptions <- parseDctermsDescriptions
      } yield Note(dcTitles, dctermsTitles, dctermsDescriptions)
    }
  }

  def parseDcTitles: XmlParser[Seq[String]] = {
    collect(stringNode("title", dc))
  }

  def parseDctermsTitles: XmlParser[Seq[String]] = {
    collect(stringNode("title", dcterms))
  }

  def parseDctermsDescriptions: XmlParser[Seq[String]] = {
    collect(stringNode("description", dcterms))
  }
}
