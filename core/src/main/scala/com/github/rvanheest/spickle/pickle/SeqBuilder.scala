package com.github.rvanheest.spickle.pickle

import scala.language.higherKinds

class SeqBuilder[State, X, Y](pickleA: Pickle[State, X], f: Y => X) {
  def map(g: X => Y): Pickle[State, Y] = {
    Pickle(
      serializer = pickleA.serializer.contramap(f),
      parser = pickleA.parser.map(g))
  }

  def flatMap(g: X => Pickle[State, Y]): Pickle[State, Y] = {
    Pickle(
      serializer = pickleA.serializer.contramapCombine(f, g(_).serializer),
      parser = pickleA.parser.flatMap(g(_).parser))
  }
}
