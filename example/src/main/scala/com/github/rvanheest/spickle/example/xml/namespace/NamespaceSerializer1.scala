package com.github.rvanheest.spickle.example.xml.namespace

import com.github.rvanheest.spickle.example.xml.namespace.Namespace.Note
import com.github.rvanheest.spickle.serializer.xml.XmlSerializer._

import scala.util.Success
import scala.xml._

object NamespaceSerializer1Runner extends App {

  val input = Note(
    Seq("dc-title1", "dc-title2", "dc-title3"),
    Seq("dcterms-title1"),
    Seq("dcterms-description1", "dcterms-description2", "dcterms-description3")
  )
  println(input)

  val Success(Seq(xml1)) = NamespaceSerializer1.serializeNamespacedNote.serialize(input)
  println(new PrettyPrinter(160, 2).format(xml1))
}

object NamespaceSerializer1 {

  val default: NamespaceBinding = NamespaceBinding(null, "http://www.github.com/rvanheest/spickle/example/namespace", TopScope)
  val dc: NamespaceBinding = NamespaceBinding("dc", "http://purl.org/dc/elements/1.1/", TopScope)
  val dcterms: NamespaceBinding = NamespaceBinding("dcterms", "http://purl.org/dc/terms/", TopScope)
  val xsi: NamespaceBinding = NamespaceBinding("xsi", "http://www.w3.org/2001/XMLSchema-instance", TopScope)

  val namespace: NamespaceBinding = NamespaceBinding(null, "http://www.github.com/rvanheest/spickle/example/namespace", NamespaceBinding("xsi", "http://www.w3.org/2001/XMLSchema-instance", NamespaceBinding("dc", "http://purl.org/dc/elements/1.1/", NamespaceBinding("dcterms", "http://purl.org/dc/terms/", TopScope))))

  def serializeNamespacedNote: XmlSerializer[Note] = {
    withNamespace(namespace) {
      attribute("noNamespaceSchemaLocation", xsi).contramap[Note](_ => "schema.xsd")
        .combine(serializeNote)
    }
  }

  def serializeNote: XmlSerializer[Note] = {
    branchNode("note", default) {
      serializeDcTitles.contramap[Note](_.dcTitle)
        .combine(serializeDctermsTitles.contramap(_.dctermsTitle))
        .combine(serializeDctermsDescription.contramap(_.dctermsDescription))
    }
  }

  def serializeDcTitles: XmlSerializer[Seq[String]] = {
    collect(stringNode("title", dc))
  }

  def serializeDctermsTitles: XmlSerializer[Seq[String]] = {
    collect(stringNode("title", dcterms))
  }

  def serializeDctermsDescription: XmlSerializer[Seq[String]] = {
    collect(stringNode("description", dcterms))
  }
}
