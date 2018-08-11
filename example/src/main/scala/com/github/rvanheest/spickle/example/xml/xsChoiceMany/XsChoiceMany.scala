package com.github.rvanheest.spickle.example.xml.xsChoiceMany

object XsChoiceMany {

  type A = String
  type B = String
  type C1 = String
  type C2 = String
  type D1 = String
  type D2A = String
  type D2B = Option[String]

  case class CHolder(c1: C1, c2: C1)

  abstract class DHolder
  case class D1Holder(d1: D1) extends DHolder
  case class D2Holder(d2A: D2A, d2B: D2B) extends DHolder

  case class EHolder(a: A)
  type FHolder = EHolder

  case class Data(e: EHolder, a: A, as: Seq[A], bs: Seq[B], cs: Seq[CHolder], ds: Seq[DHolder], f: FHolder) {
    override def toString: String = {
      s"""Mixed
         |  e:  $e
         |  a:  $a
         |  as: ${ if (as.isEmpty) "<none>" else as.mkString("\n    - ", "\n    - ", "") }
         |  bs: ${ if (bs.isEmpty) "<none>" else bs.mkString("\n    - ", "\n    - ", "") }
         |  cs: ${ if (cs.isEmpty) "<none>" else cs.mkString("\n    - ", "\n    - ", "") }
         |  ds: ${ if (ds.isEmpty) "<none>" else ds.mkString("\n    - ", "\n    - ", "") }
         |  f:  $f""".stripMargin
    }
  }
}
