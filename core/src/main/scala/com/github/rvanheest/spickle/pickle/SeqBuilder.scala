package com.github.rvanheest.spickle.pickle

import scala.language.higherKinds
import scala.util.Try

class SeqBuilder[X, Y, State](pickleA: Pickle[X, State], f: Y => X) {
	def map(g: X => Y): Pickle[Y, State] = {
		Pickle(
			pickle = (y, state) => {
				for {
					x <- Try { f(y) }
					state2 <- pickleA.pickle(x, state)
				} yield state2
			},
			unpickle = pickleA.parse.map(g).parse)
	}

	def flatMap(g: X => Pickle[Y, State]): Pickle[Y, State] = {
		Pickle(
			pickle = (y, state) => {
				for {
					x <- Try { f(y) }
					state2 <- g(x).pickle(y, state)
					state3 <- pickleA.pickle(x, state2)
				} yield state3
			},
			unpickle = pickleA.parse.flatMap(g(_).parse).parse)
	}
}
