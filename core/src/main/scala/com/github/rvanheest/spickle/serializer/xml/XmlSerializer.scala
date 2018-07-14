package com.github.rvanheest.spickle.serializer.xml

import com.github.rvanheest.spickle.serializer.{ Serializer, SerializerFailedException }

import scala.util.{ Failure, Success, Try }
import scala.xml._

object XmlSerializer {

  type XmlSerializer[A] = Serializer[Seq[Node], A]

  def emptyNode(name: String): XmlSerializer[Unit] = {
    node(name).contramap[Unit](_ => <xml/>.copy(label = name))
  }

  def node(name: String): XmlSerializer[Node] = {
    Serializer {
      case (head, tail) if head.label == name => Success(head ++ tail)
      case (head, _) => Failure(new NoSuchElementException(s"element '$head' does not contain an element with name '$name'"))
    }
  }

  def stringNode(name: String): XmlSerializer[String] = {
    node(name).contramap[String](s => <xml>{s}</xml>.copy(label = name))
  }

  def branchNode[A](name: String)(serializerA: XmlSerializer[A]): XmlSerializer[A] = {
    Serializer((a, xml) => serializerA.serialize(a, NodeSeq.Empty).map(nodes => <xml>{nodes}</xml>.copy(label = name) ++ xml))
  }

  def attribute(name: String): XmlSerializer[String] = {
    Serializer((s: String, xml: Seq[Node]) => Try {
      xml.headOption map {
        case elem: Elem => elem % new UnprefixedAttribute(name, s, Null) ++ xml.tail
        case x => throw SerializerFailedException(s"Can only add an attribute with name '$name' to elements: $x")
      } getOrElse {
        throw SerializerFailedException(s"Cannot add an attribute with name '$name' to an empty node sequence")
      }
    })
  }

  def namespaceAttribute(name: String)(implicit namespace: NamespaceBinding): XmlSerializer[String] = {
    Serializer((s: String, xml: Seq[Node]) => Try {
      xml.headOption map {
        case elem: Elem => elem % new PrefixedAttribute(namespace.prefix, name, s, Null) ++ xml.tail
        case x => throw SerializerFailedException(s"Can only add an attribute with name '${ namespace.prefix }:$name' to elements: $x")
      } getOrElse {
        throw SerializerFailedException(s"Cannot add an attribute with name '${ namespace.prefix }:$name' to an empty node sequence")
      }
    })
  }

  def all[T1, S1](s1: XmlSerializer[T1])(f1: S1 => Option[T1]): XmlSerializer[S1] = {
    Serializer((ts, xml) => s1.maybe.serialize(f1(ts), xml))
  }

  def all[T1, S1, T2, S2](p1: XmlSerializer[T1],
                          p2: XmlSerializer[T2],
                         )
                         (f1: S1 => Option[T1],
                          f2: S2 => Option[T2],
                         ): XmlSerializer[(S1, S2)] = {
    type GroupedOption = (Option[T1], Option[T2])
    Serializer {
      case ((ts, rs), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r) => r })
          .serialize((f1(ts), f2(rs)), xml)
    }
  }

  def all[T1, S1, T2, S2, T3, S3](p1: XmlSerializer[T1],
                                  p2: XmlSerializer[T2],
                                  p3: XmlSerializer[T3],
                                 )
                                 (f1: S1 => Option[T1],
                                  f2: S2 => Option[T2],
                                  f3: S3 => Option[T3],
                                 ): XmlSerializer[(S1, S2, S3)] = {
    type GroupedOption = (Option[T1], Option[T2], Option[T3])
    Serializer {
      case ((ts, rs, ss), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r, _) => r })
          .combine(p3.maybe.contramap[GroupedOption] { case (_, _, s) => s })
          .serialize((f1(ts), f2(rs), f3(ss)), xml)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4](p1: XmlSerializer[T1],
                                          p2: XmlSerializer[T2],
                                          p3: XmlSerializer[T3],
                                          p4: XmlSerializer[T4],
                                         )
                                         (f1: S1 => Option[T1],
                                          f2: S2 => Option[T2],
                                          f3: S3 => Option[T3],
                                          f4: S4 => Option[T4],
                                         ): XmlSerializer[(S1, S2, S3, S4)] = {
    type GroupedOption = (Option[T1], Option[T2], Option[T3], Option[T4])
    Serializer {
      case ((ts, rs, ss, vs), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _, _, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r, _, _) => r })
          .combine(p3.maybe.contramap[GroupedOption] { case (_, _, s, _) => s })
          .combine(p4.maybe.contramap[GroupedOption] { case (_, _, _, v) => v })
          .serialize((f1(ts), f2(rs), f3(ss), f4(vs)), xml)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5](p1: XmlSerializer[T1],
                                                  p2: XmlSerializer[T2],
                                                  p3: XmlSerializer[T3],
                                                  p4: XmlSerializer[T4],
                                                  p5: XmlSerializer[T5],
                                                 )
                                                 (f1: S1 => Option[T1],
                                                  f2: S2 => Option[T2],
                                                  f3: S3 => Option[T3],
                                                  f4: S4 => Option[T4],
                                                  f5: S5 => Option[T5],
                                                 ): XmlSerializer[(S1, S2, S3, S4, S5)] = {
    type GroupedOption = (Option[T1], Option[T2], Option[T3], Option[T4], Option[T5])
    Serializer {
      case ((ts, rs, ss, vs, ws), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _, _, _, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r, _, _, _) => r })
          .combine(p3.maybe.contramap[GroupedOption] { case (_, _, s, _, _) => s })
          .combine(p4.maybe.contramap[GroupedOption] { case (_, _, _, v, _) => v })
          .combine(p5.maybe.contramap[GroupedOption] { case (_, _, _, _, w) => w })
          .serialize((f1(ts), f2(rs), f3(ss), f4(vs), f5(ws)), xml)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6](p1: XmlSerializer[T1],
          p2: XmlSerializer[T2],
          p3: XmlSerializer[T3],
          p4: XmlSerializer[T4],
          p5: XmlSerializer[T5],
          p6: XmlSerializer[T6],
         )
         (f1: S1 => Option[T1],
          f2: S2 => Option[T2],
          f3: S3 => Option[T3],
          f4: S4 => Option[T4],
          f5: S5 => Option[T5],
          f6: S6 => Option[T6],
         ): XmlSerializer[(S1, S2, S3, S4, S5, S6)] = {
    type GroupedOption = (Option[T1], Option[T2], Option[T3], Option[T4], Option[T5], Option[T6])
    Serializer {
      case ((ts, rs, ss, vs, ws, xs), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _, _, _, _, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r, _, _, _, _) => r })
          .combine(p3.maybe.contramap[GroupedOption] { case (_, _, s, _, _, _) => s })
          .combine(p4.maybe.contramap[GroupedOption] { case (_, _, _, v, _, _) => v })
          .combine(p5.maybe.contramap[GroupedOption] { case (_, _, _, _, w, _) => w })
          .combine(p6.maybe.contramap[GroupedOption] { case (_, _, _, _, _, x) => x })
          .serialize((f1(ts), f2(rs), f3(ss), f4(vs), f5(ws), f6(xs)), xml)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7](p1: XmlSerializer[T1],
                  p2: XmlSerializer[T2],
                  p3: XmlSerializer[T3],
                  p4: XmlSerializer[T4],
                  p5: XmlSerializer[T5],
                  p6: XmlSerializer[T6],
                  p7: XmlSerializer[T7],
                 )
                 (f1: S1 => Option[T1],
                  f2: S2 => Option[T2],
                  f3: S3 => Option[T3],
                  f4: S4 => Option[T4],
                  f5: S5 => Option[T5],
                  f6: S6 => Option[T6],
                  f7: S7 => Option[T7],
                 ): XmlSerializer[(S1, S2, S3, S4, S5, S6, S7)] = {
    type GroupedOption = (Option[T1], Option[T2], Option[T3], Option[T4], Option[T5], Option[T6], Option[T7])
    Serializer {
      case ((ts, rs, ss, vs, ws, xs, ys), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _, _, _, _, _, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r, _, _, _, _, _) => r })
          .combine(p3.maybe.contramap[GroupedOption] { case (_, _, s, _, _, _, _) => s })
          .combine(p4.maybe.contramap[GroupedOption] { case (_, _, _, v, _, _, _) => v })
          .combine(p5.maybe.contramap[GroupedOption] { case (_, _, _, _, w, _, _) => w })
          .combine(p6.maybe.contramap[GroupedOption] { case (_, _, _, _, _, x, _) => x })
          .combine(p7.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, y) => y })
          .serialize((f1(ts), f2(rs), f3(ss), f4(vs), f5(ws), f6(xs), f7(ys)), xml)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8](p1: XmlSerializer[T1],
                          p2: XmlSerializer[T2],
                          p3: XmlSerializer[T3],
                          p4: XmlSerializer[T4],
                          p5: XmlSerializer[T5],
                          p6: XmlSerializer[T6],
                          p7: XmlSerializer[T7],
                          p8: XmlSerializer[T8],
                         )
                         (f1: S1 => Option[T1],
                          f2: S2 => Option[T2],
                          f3: S3 => Option[T3],
                          f4: S4 => Option[T4],
                          f5: S5 => Option[T5],
                          f6: S6 => Option[T6],
                          f7: S7 => Option[T7],
                          f8: S8 => Option[T8],
                         ): XmlSerializer[(S1, S2, S3, S4, S5, S6, S7, S8)] = {
    type GroupedOption = (Option[T1], Option[T2], Option[T3], Option[T4], Option[T5],
      Option[T6], Option[T7], Option[T8])
    Serializer {
      case ((ts, rs, ss, vs, ws, xs, ys, zs), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _, _, _, _, _, _, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r, _, _, _, _, _, _) => r })
          .combine(p3.maybe.contramap[GroupedOption] { case (_, _, s, _, _, _, _, _) => s })
          .combine(p4.maybe.contramap[GroupedOption] { case (_, _, _, v, _, _, _, _) => v })
          .combine(p5.maybe.contramap[GroupedOption] { case (_, _, _, _, w, _, _, _) => w })
          .combine(p6.maybe.contramap[GroupedOption] { case (_, _, _, _, _, x, _, _) => x })
          .combine(p7.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, y, _) => y })
          .combine(p8.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, z) => z })
          .serialize((f1(ts), f2(rs), f3(ss), f4(vs), f5(ws), f6(xs), f7(ys), f8(zs)), xml)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8, T9, S9](p1: XmlSerializer[T1],
                                  p2: XmlSerializer[T2],
                                  p3: XmlSerializer[T3],
                                  p4: XmlSerializer[T4],
                                  p5: XmlSerializer[T5],
                                  p6: XmlSerializer[T6],
                                  p7: XmlSerializer[T7],
                                  p8: XmlSerializer[T8],
                                  p9: XmlSerializer[T9],
                                 )
                                 (f1: S1 => Option[T1],
                                  f2: S2 => Option[T2],
                                  f3: S3 => Option[T3],
                                  f4: S4 => Option[T4],
                                  f5: S5 => Option[T5],
                                  f6: S6 => Option[T6],
                                  f7: S7 => Option[T7],
                                  f8: S8 => Option[T8],
                                  f9: S9 => Option[T9],
                                 ): XmlSerializer[(S1, S2, S3, S4, S5, S6, S7, S8, S9)] = {
    type GroupedOption = (Option[T1], Option[T2], Option[T3], Option[T4], Option[T5],
      Option[T6], Option[T7], Option[T8], Option[T9])
    Serializer {
      case ((ts, rs, ss, vs, ws, xs, ys, zs, as), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _, _, _, _, _, _, _, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r, _, _, _, _, _, _, _) => r })
          .combine(p3.maybe.contramap[GroupedOption] { case (_, _, s, _, _, _, _, _, _) => s })
          .combine(p4.maybe.contramap[GroupedOption] { case (_, _, _, v, _, _, _, _, _) => v })
          .combine(p5.maybe.contramap[GroupedOption] { case (_, _, _, _, w, _, _, _, _) => w })
          .combine(p6.maybe.contramap[GroupedOption] { case (_, _, _, _, _, x, _, _, _) => x })
          .combine(p7.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, y, _, _) => y })
          .combine(p8.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, z, _) => z })
          .combine(p9.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, _, a) => a })
          .serialize((f1(ts), f2(rs), f3(ss), f4(vs), f5(ws), f6(xs), f7(ys), f8(zs), f9(as)), xml)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8, T9, S9, T10, S10](p1: XmlSerializer[T1],
                                            p2: XmlSerializer[T2],
                                            p3: XmlSerializer[T3],
                                            p4: XmlSerializer[T4],
                                            p5: XmlSerializer[T5],
                                            p6: XmlSerializer[T6],
                                            p7: XmlSerializer[T7],
                                            p8: XmlSerializer[T8],
                                            p9: XmlSerializer[T9],
                                            p10: XmlSerializer[T10],
                                           )
                                           (f1: S1 => Option[T1],
                                            f2: S2 => Option[T2],
                                            f3: S3 => Option[T3],
                                            f4: S4 => Option[T4],
                                            f5: S5 => Option[T5],
                                            f6: S6 => Option[T6],
                                            f7: S7 => Option[T7],
                                            f8: S8 => Option[T8],
                                            f9: S9 => Option[T9],
                                            f10: S10 => Option[T10],
                                           ): XmlSerializer[(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10)] = {
    type GroupedOption = (Option[T1], Option[T2], Option[T3], Option[T4], Option[T5],
      Option[T6], Option[T7], Option[T8], Option[T9], Option[T10])
    Serializer {
      case ((ts, rs, ss, vs, ws, xs, ys, zs, as, bs), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _, _, _, _, _, _, _, _, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r, _, _, _, _, _, _, _, _) => r })
          .combine(p3.maybe.contramap[GroupedOption] { case (_, _, s, _, _, _, _, _, _, _) => s })
          .combine(p4.maybe.contramap[GroupedOption] { case (_, _, _, v, _, _, _, _, _, _) => v })
          .combine(p5.maybe.contramap[GroupedOption] { case (_, _, _, _, w, _, _, _, _, _) => w })
          .combine(p6.maybe.contramap[GroupedOption] { case (_, _, _, _, _, x, _, _, _, _) => x })
          .combine(p7.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, y, _, _, _) => y })
          .combine(p8.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, z, _, _) => z })
          .combine(p9.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, _, a, _) => a })
          .combine(p10.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, _, _, b) => b })
          .serialize((f1(ts), f2(rs), f3(ss), f4(vs), f5(ws), f6(xs), f7(ys), f8(zs), f9(as), f10(bs)), xml)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8, T9, S9, T10, S10,
  T11, S11](p1: XmlSerializer[T1],
            p2: XmlSerializer[T2],
            p3: XmlSerializer[T3],
            p4: XmlSerializer[T4],
            p5: XmlSerializer[T5],
            p6: XmlSerializer[T6],
            p7: XmlSerializer[T7],
            p8: XmlSerializer[T8],
            p9: XmlSerializer[T9],
            p10: XmlSerializer[T10],
            p11: XmlSerializer[T11],
           )
           (f1: S1 => Option[T1],
            f2: S2 => Option[T2],
            f3: S3 => Option[T3],
            f4: S4 => Option[T4],
            f5: S5 => Option[T5],
            f6: S6 => Option[T6],
            f7: S7 => Option[T7],
            f8: S8 => Option[T8],
            f9: S9 => Option[T9],
            f10: S10 => Option[T10],
            f11: S11 => Option[T11],
           ): XmlSerializer[(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11)] = {
    type GroupedOption = (Option[T1], Option[T2], Option[T3], Option[T4], Option[T5],
      Option[T6], Option[T7], Option[T8], Option[T9], Option[T10],
      Option[T11])
    Serializer {
      case ((ts, rs, ss, vs, ws, xs, ys, zs, as, bs, cs), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _, _, _, _, _, _, _, _, _, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r, _, _, _, _, _, _, _, _, _) => r })
          .combine(p3.maybe.contramap[GroupedOption] { case (_, _, s, _, _, _, _, _, _, _, _) => s })
          .combine(p4.maybe.contramap[GroupedOption] { case (_, _, _, v, _, _, _, _, _, _, _) => v })
          .combine(p5.maybe.contramap[GroupedOption] { case (_, _, _, _, w, _, _, _, _, _, _) => w })
          .combine(p6.maybe.contramap[GroupedOption] { case (_, _, _, _, _, x, _, _, _, _, _) => x })
          .combine(p7.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, y, _, _, _, _) => y })
          .combine(p8.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, z, _, _, _) => z })
          .combine(p9.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, _, a, _, _) => a })
          .combine(p10.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, _, _, b, _) => b })
          .combine(p11.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, _, _, _, c) => c })
          .serialize((f1(ts), f2(rs), f3(ss), f4(vs), f5(ws), f6(xs), f7(ys), f8(zs), f9(as), f10(bs), f11(cs)), xml)
    }
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8, T9, S9, T10, S10,
  T11, S11, T12, S12](p1: XmlSerializer[T1],
                      p2: XmlSerializer[T2],
                      p3: XmlSerializer[T3],
                      p4: XmlSerializer[T4],
                      p5: XmlSerializer[T5],
                      p6: XmlSerializer[T6],
                      p7: XmlSerializer[T7],
                      p8: XmlSerializer[T8],
                      p9: XmlSerializer[T9],
                      p10: XmlSerializer[T10],
                      p11: XmlSerializer[T11],
                      p12: XmlSerializer[T12],
                     )
                     (f1: S1 => Option[T1],
                      f2: S2 => Option[T2],
                      f3: S3 => Option[T3],
                      f4: S4 => Option[T4],
                      f5: S5 => Option[T5],
                      f6: S6 => Option[T6],
                      f7: S7 => Option[T7],
                      f8: S8 => Option[T8],
                      f9: S9 => Option[T9],
                      f10: S10 => Option[T10],
                      f11: S11 => Option[T11],
                      f12: S12 => Option[T12],
                     ): XmlSerializer[(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12)] = {
    type GroupedOption = (Option[T1], Option[T2], Option[T3], Option[T4], Option[T5],
      Option[T6], Option[T7], Option[T8], Option[T9], Option[T10],
      Option[T11], Option[T12])
    Serializer {
      case ((ts, rs, ss, vs, ws, xs, ys, zs, as, bs, cs, ds), xml: Seq[Node]) =>
        p1.maybe.contramap[GroupedOption] { case (t, _, _, _, _, _, _, _, _, _, _, _) => t }
          .combine(p2.maybe.contramap[GroupedOption] { case (_, r, _, _, _, _, _, _, _, _, _, _) => r })
          .combine(p3.maybe.contramap[GroupedOption] { case (_, _, s, _, _, _, _, _, _, _, _, _) => s })
          .combine(p4.maybe.contramap[GroupedOption] { case (_, _, _, v, _, _, _, _, _, _, _, _) => v })
          .combine(p5.maybe.contramap[GroupedOption] { case (_, _, _, _, w, _, _, _, _, _, _, _) => w })
          .combine(p6.maybe.contramap[GroupedOption] { case (_, _, _, _, _, x, _, _, _, _, _, _) => x })
          .combine(p7.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, y, _, _, _, _, _) => y })
          .combine(p8.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, z, _, _, _, _) => z })
          .combine(p9.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, _, a, _, _, _) => a })
          .combine(p10.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, _, _, b, _, _) => b })
          .combine(p11.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, _, _, _, c, _) => c })
          .combine(p12.maybe.contramap[GroupedOption] { case (_, _, _, _, _, _, _, _, _, _, _, d) => d })
          .serialize((f1(ts), f2(rs), f3(ss), f4(vs), f5(ws), f6(xs), f7(ys), f8(zs), f9(as), f10(bs), f11(cs), f12(ds)), xml)
    }
  }

  def optional[X]: Option[X] => Option[X] = identity

  def mandatory[X]: X => Option[X] = Option(_)
}
