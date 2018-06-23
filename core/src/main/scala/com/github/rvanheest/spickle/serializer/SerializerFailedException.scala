package com.github.rvanheest.spickle.serializer

case class SerializerFailedException(msg: String) extends Exception(msg) {
  def this(msg: String, cause: Throwable) = {
    this(msg)
    initCause(cause)
  }
}
