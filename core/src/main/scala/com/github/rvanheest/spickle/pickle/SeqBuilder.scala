package com.github.rvanheest.spickle.pickle

import com.github.rvanheest.spickle.serializer.Serializer

import scala.language.higherKinds
import scala.util.Try

class SeqBuilder[State, X, Y](pickleA: Pickle[State, X], f: Y => X) {
  def map(g: X => Y): Pickle[State, Y] = {
    Pickle(
      serializer = pickleA.serializer.contramap(f),
      parser = pickleA.parser.map(g))
  }

  def flatMap(g: X => Pickle[State, Y]): Pickle[State, Y] = {
    Pickle(
      serializer = Serializer((y, state) => {
        for {
          x <- Try { f(y) }
          state2 <- g(x).serialize(y, state)
          state3 <- pickleA.serialize(x, state2)
        } yield state3
      }),
      parser = pickleA.parser.flatMap(g(_).parser))
  }
}
