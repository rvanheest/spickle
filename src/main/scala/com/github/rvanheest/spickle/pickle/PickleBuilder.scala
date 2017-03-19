package com.github.rvanheest.spickle.pickle

import scala.util.Try

trait PickleBuilder[A, State, Repr] {
	def apply(pickle: (A, State) => Try[State], unpickle: State => (Try[A], State)): Repr
}
