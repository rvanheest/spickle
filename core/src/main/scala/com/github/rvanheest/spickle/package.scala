package com.github.rvanheest

import shapeless.Generic

import scala.language.reflectiveCalls

package object spickle {

  type MyGeneric[T, R] = Generic[T] { type Repr = R }
}
