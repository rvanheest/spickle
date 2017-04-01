package com.github.rvanheest.spickle.pickle

case class PickleFailedException(msg: String) extends Exception(msg) {
  def this(msg: String, cause: Throwable) = {
    this(msg)
    initCause(cause)
  }
}
