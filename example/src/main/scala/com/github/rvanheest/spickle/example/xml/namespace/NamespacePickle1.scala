package com.github.rvanheest.spickle.example.xml.namespace

import java.nio.file.Paths

import com.github.rvanheest.spickle.example.xml.namespace.Namespace.Note
import com.github.rvanheest.spickle.pickle.xml.XmlPickle._

import scala.util.Success
import scala.xml._

object NamespacePickle1Runner extends App {

  val pickle = NamespacePickle1.pickleNamespacedNote

  val path = Paths.get(getClass.getResource("/namespace/note1.xml").toURI)
  val xml = Utility.trim(XML.loadFile(path.toFile))

  println(new PrettyPrinter(160, 2).format(xml))

  val (Success(note), remainder) = pickle.parse(xml)
  println(s"result: $note")
  println(s"remainder: ${ if (remainder.isEmpty) "<empty>" else remainder }")

  val Success(Seq(resultXml)) = pickle.serialize(note)
  println(new PrettyPrinter(160, 2).format(resultXml))
}

object NamespacePickle1 {

  val dc: NamespaceBinding = NamespaceBinding("dc", "http://purl.org/dc/elements/1.1/", TopScope)
  val dcterms: NamespaceBinding = NamespaceBinding("dcterms", "http://purl.org/dc/terms/", TopScope)
  val xsi: NamespaceBinding = NamespaceBinding("xsi", "http://www.w3.org/2001/XMLSchema-instance", TopScope)

  val namespace: NamespaceBinding = NamespaceBinding("xsi", "http://www.w3.org/2001/XMLSchema-instance", NamespaceBinding("dc", "http://purl.org/dc/elements/1.1/", NamespaceBinding("dcterms", "http://purl.org/dc/terms/", TopScope)))

  def pickleNamespacedNote: XmlPickle[Note] = {
    withNamespace(namespace) {
      for {
        _ <- attribute("noNamespaceSchemaLocation", xsi).seq[Note](_ => "scheme.xsd")
        note <- pickleNote.seqId
      } yield note
    }
  }

  def pickleNote: XmlPickle[Note] = {
    branchNode("note") {
      for {
        dcTitles <- pickleDcTitles.seq[Note](_.dcTitle)
        dctermsTitles <- pickleDctermsTitles.seq[Note](_.dctermsTitle)
        dctermsDescription <- pickleDctermsDescription.seq[Note](_.dctermsDescription)
      } yield Note(dcTitles, dctermsTitles, dctermsDescription)
    }
  }

  def pickleDcTitles: XmlPickle[Seq[String]] = {
    collect(stringNode("title", dc))
  }

  def pickleDctermsTitles: XmlPickle[Seq[String]] = {
    collect(stringNode("title", dcterms))
  }

  def pickleDctermsDescription: XmlPickle[Seq[String]] = {
    collect(stringNode("description", dcterms))
  }
}
