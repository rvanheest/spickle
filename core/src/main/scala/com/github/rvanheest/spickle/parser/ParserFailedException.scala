package com.github.rvanheest.spickle.parser

case class ParserFailedException(msg: String) extends Exception(msg) {
  def this(msg: String, cause: Throwable) = {
    this(msg)
    initCause(cause)
  }
}
