package com.github.rvanheest.spickle.example.xml.namespace

object Namespace {

  case class Note(dcTitle: Seq[String],
                  dctermsTitle: Seq[String],
                  dctermsDescription: Seq[String])
}
