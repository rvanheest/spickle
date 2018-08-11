package com.github.rvanheest.spickle.example.xml.all

object All {

  case class Numbers(a: Option[Int], b: Option[Int], c: Option[Int], d: Option[Int], e: Int,
                     f: Int, g: Int, h: Int, i: Int, j: Int,
                     k: Int, l: Int, m: Int, n: Int, o: Int,
                     p: Int, q: Int, r: Int, s: Int, t: Int,
                     u: Int, v: Int, w: Int, x: Int, y: Int,
                     z: Int)

  case class Person(firstName: String, lastName: String, age: Int)
}
