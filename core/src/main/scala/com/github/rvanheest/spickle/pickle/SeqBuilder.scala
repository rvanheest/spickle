package com.github.rvanheest.spickle.pickle

import scala.language.higherKinds
import scala.util.Try

class SeqBuilder[X, Y, State, Repr[_]](pickleA: Pickle[X, State], f: Y => X)(implicit builder: PickleBuilder[Y, State, Repr[Y]]) {
	def map(g: X => Y): Repr[Y] = {
		builder(
			pickle = (y, state) => {
				for {
					x <- Try { f(y) }
					state2 <- pickleA.pickle(x, state)
				} yield state2
			},
			unpickle = pickleA.parse.map(g).run)
	}

	def flatMap(g: X => Pickle[Y, State]): Repr[Y] = {
		builder(
			pickle = (y, state) => {
				for {
					x <- Try { f(y) }
					state2 <- g(x).pickle(y, state)
					state3 <- pickleA.pickle(x, state2)
				} yield state3
			},
			unpickle = pickleA.parse.flatMap(g(_).parse).run)
	}
}
