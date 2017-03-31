package com.github.rvanheest.spickle.pickle

import scala.language.higherKinds
import scala.util.Try

class SeqBuilder[X, Y, State](pickleA: Pickle[X, State], f: Y => X) {
  def map(g: X => Y): Pickle[Y, State] = {
    Pickle(
      pickler = (y, state) => {
        for {
          x <- Try { f(y) }
          state2 <- pickleA.pickler(x, state)
        } yield state2
      },
      parser = pickleA.parser.map(g))
  }

  def flatMap(g: X => Pickle[Y, State]): Pickle[Y, State] = {
    Pickle(
      pickler = (y, state) => {
        for {
          x <- Try { f(y) }
          state2 <- g(x).pickler(y, state)
          state3 <- pickleA.pickler(x, state2)
        } yield state3
      },
      parser = pickleA.parser.flatMap(g(_).parser))
  }
}
