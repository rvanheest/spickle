package com.github.rvanheest.spickle.pickle.xml

import com.github.rvanheest.spickle.parser.xml.XmlParser
import com.github.rvanheest.spickle.pickle.Pickle
import com.github.rvanheest.spickle.serializer.xml.XmlSerializer
import shapeless.{ ::, HNil }

import scala.language.reflectiveCalls
import scala.xml._

object XmlPickle {

  type XmlPickle[A] = Pickle[Seq[Node], A]

  def emptyNode(name: String): XmlPickle[Unit] = {
    Pickle(
      serializer = XmlSerializer.emptyNode(name),
      parser = XmlParser.emptyNode(name)
    )
  }

  def node(name: String): XmlPickle[Node] = {
    Pickle(
      serializer = XmlSerializer.node(name),
      parser = XmlParser.node(name)
    )
  }

  def stringNode(name: String): XmlPickle[String] = {
    Pickle(
      serializer = XmlSerializer.stringNode(name),
      parser = XmlParser.stringNode(name)
    )
  }

  def branchNode[A](name: String)(pickleA: XmlPickle[A]): XmlPickle[A] = {
    Pickle(
      serializer = XmlSerializer.branchNode(name)(pickleA.serializer),
      parser = XmlParser.branchNode(name)(pickleA.parser)
    )
  }

  def attribute(name: String): XmlPickle[String] = {
    Pickle(
      serializer = XmlSerializer.attribute(name),
      parser = XmlParser.attribute(name)
    )
  }

  def namespaceAttribute(name: String)(implicit namespace: NamespaceBinding): XmlPickle[String] = {
    Pickle(
      serializer = XmlSerializer.namespaceAttribute(name),
      parser = XmlParser.namespaceAttribute(name)
    )
  }

  def fromAllMandatory[T](parser: XmlPickle[T]): AllPickleBuilder[T :: HNil] = {
    AllPickleBuilder.fromMandatory(parser)
  }

  def fromAllOptional[T](parser: XmlPickle[T]): AllPickleBuilder[Option[T] :: HNil] = {
    AllPickleBuilder.fromOptional(parser)
  }

  case class ISO[X, XS](run: X => XS, undo: XS => X)

  // TODO remove all parsers
  def all[T1, S1](p1: XmlPickle[T1])(f1: ISO[Option[T1], S1]): XmlPickle[S1] = {
    Pickle(
      serializer = XmlSerializer.all(p1.serializer)(f1.undo),
      parser = XmlParser.all(p1.parser)(f1.run)
    )
  }

  def all[T1, S1, T2, S2](p1: XmlPickle[T1],
                          p2: XmlPickle[T2],
                         )
                         (f1: ISO[Option[T1], S1],
                          f2: ISO[Option[T2], S2],
                         ): XmlPickle[(S1, S2)] = {
    Pickle(
      serializer = XmlSerializer.all(p1.serializer, p2.serializer)(f1.undo, f2.undo),
      parser = XmlParser.all(p1.parser, p2.parser)(f1.run, f2.run)
    )
  }

  def all[T1, S1, T2, S2, T3, S3](p1: XmlPickle[T1],
                                  p2: XmlPickle[T2],
                                  p3: XmlPickle[T3],
                                 )
                                 (f1: ISO[Option[T1], S1],
                                  f2: ISO[Option[T2], S2],
                                  f3: ISO[Option[T3], S3],
                                 ): XmlPickle[(S1, S2, S3)] = {
    Pickle(
      serializer = XmlSerializer.all(p1.serializer, p2.serializer, p3.serializer)(f1.undo, f2.undo, f3.undo),
      parser = XmlParser.all(p1.parser, p2.parser, p3.parser)(f1.run, f2.run, f3.run)
    )
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4](p1: XmlPickle[T1],
                                          p2: XmlPickle[T2],
                                          p3: XmlPickle[T3],
                                          p4: XmlPickle[T4],
                                         )
                                         (f1: ISO[Option[T1], S1],
                                          f2: ISO[Option[T2], S2],
                                          f3: ISO[Option[T3], S3],
                                          f4: ISO[Option[T4], S4],
                                         ): XmlPickle[(S1, S2, S3, S4)] = {
    Pickle(
      serializer = XmlSerializer.all(
        p1.serializer, p2.serializer, p3.serializer, p4.serializer
      )(f1.undo, f2.undo, f3.undo, f4.undo
      ),
      parser = XmlParser.all(
        p1.parser, p2.parser, p3.parser, p4.parser
      )(f1.run, f2.run, f3.run, f4.run
      )
    )
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5](p1: XmlPickle[T1],
                                                  p2: XmlPickle[T2],
                                                  p3: XmlPickle[T3],
                                                  p4: XmlPickle[T4],
                                                  p5: XmlPickle[T5],
                                                 )
                                                 (f1: ISO[Option[T1], S1],
                                                  f2: ISO[Option[T2], S2],
                                                  f3: ISO[Option[T3], S3],
                                                  f4: ISO[Option[T4], S4],
                                                  f5: ISO[Option[T5], S5],
                                                 ): XmlPickle[(S1, S2, S3, S4, S5)] = {
    Pickle(
      serializer = XmlSerializer.all(
        p1.serializer, p2.serializer, p3.serializer, p4.serializer, p5.serializer
      )(f1.undo, f2.undo, f3.undo, f4.undo, f5.undo
      ),
      parser = XmlParser.all(
        p1.parser, p2.parser, p3.parser, p4.parser, p5.parser
      )(f1.run, f2.run, f3.run, f4.run, f5.run
      )
    )
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6](p1: XmlPickle[T1],
          p2: XmlPickle[T2],
          p3: XmlPickle[T3],
          p4: XmlPickle[T4],
          p5: XmlPickle[T5],
          p6: XmlPickle[T6],
         )
         (f1: ISO[Option[T1], S1],
          f2: ISO[Option[T2], S2],
          f3: ISO[Option[T3], S3],
          f4: ISO[Option[T4], S4],
          f5: ISO[Option[T5], S5],
          f6: ISO[Option[T6], S6],
         ): XmlPickle[(S1, S2, S3, S4, S5, S6)] = {
    Pickle(
      serializer = XmlSerializer.all(
        p1.serializer, p2.serializer, p3.serializer, p4.serializer, p5.serializer,
        p6.serializer
      )(f1.undo, f2.undo, f3.undo, f4.undo, f5.undo,
        f6.undo
      ),
      parser = XmlParser.all(
        p1.parser, p2.parser, p3.parser, p4.parser, p5.parser,
        p6.parser
      )(f1.run, f2.run, f3.run, f4.run, f5.run,
        f6.run
      )
    )
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7](p1: XmlPickle[T1],
                  p2: XmlPickle[T2],
                  p3: XmlPickle[T3],
                  p4: XmlPickle[T4],
                  p5: XmlPickle[T5],
                  p6: XmlPickle[T6],
                  p7: XmlPickle[T7],
                 )
                 (f1: ISO[Option[T1], S1],
                  f2: ISO[Option[T2], S2],
                  f3: ISO[Option[T3], S3],
                  f4: ISO[Option[T4], S4],
                  f5: ISO[Option[T5], S5],
                  f6: ISO[Option[T6], S6],
                  f7: ISO[Option[T7], S7],
                 ): XmlPickle[(S1, S2, S3, S4, S5, S6, S7)] = {
    Pickle(
      serializer = XmlSerializer.all(
        p1.serializer, p2.serializer, p3.serializer, p4.serializer, p5.serializer,
        p6.serializer, p7.serializer
      )(f1.undo, f2.undo, f3.undo, f4.undo, f5.undo,
        f6.undo, f7.undo
      ),
      parser = XmlParser.all(
        p1.parser, p2.parser, p3.parser, p4.parser, p5.parser,
        p6.parser, p7.parser
      )(f1.run, f2.run, f3.run, f4.run, f5.run,
        f6.run, f7.run
      )
    )
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8](p1: XmlPickle[T1],
                          p2: XmlPickle[T2],
                          p3: XmlPickle[T3],
                          p4: XmlPickle[T4],
                          p5: XmlPickle[T5],
                          p6: XmlPickle[T6],
                          p7: XmlPickle[T7],
                          p8: XmlPickle[T8],
                         )
                         (f1: ISO[Option[T1], S1],
                          f2: ISO[Option[T2], S2],
                          f3: ISO[Option[T3], S3],
                          f4: ISO[Option[T4], S4],
                          f5: ISO[Option[T5], S5],
                          f6: ISO[Option[T6], S6],
                          f7: ISO[Option[T7], S7],
                          f8: ISO[Option[T8], S8],
                         ): XmlPickle[(S1, S2, S3, S4, S5, S6, S7, S8)] = {
    Pickle(
      serializer = XmlSerializer.all(
        p1.serializer, p2.serializer, p3.serializer, p4.serializer, p5.serializer,
        p6.serializer, p7.serializer, p8.serializer
      )(f1.undo, f2.undo, f3.undo, f4.undo, f5.undo,
        f6.undo, f7.undo, f8.undo
      ),
      parser = XmlParser.all(
        p1.parser, p2.parser, p3.parser, p4.parser, p5.parser,
        p6.parser, p7.parser, p8.parser
      )(f1.run, f2.run, f3.run, f4.run, f5.run,
        f6.run, f7.run, f8.run
      )
    )
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8, T9, S9](p1: XmlPickle[T1],
                                  p2: XmlPickle[T2],
                                  p3: XmlPickle[T3],
                                  p4: XmlPickle[T4],
                                  p5: XmlPickle[T5],
                                  p6: XmlPickle[T6],
                                  p7: XmlPickle[T7],
                                  p8: XmlPickle[T8],
                                  p9: XmlPickle[T9],
                                 )
                                 (f1: ISO[Option[T1], S1],
                                  f2: ISO[Option[T2], S2],
                                  f3: ISO[Option[T3], S3],
                                  f4: ISO[Option[T4], S4],
                                  f5: ISO[Option[T5], S5],
                                  f6: ISO[Option[T6], S6],
                                  f7: ISO[Option[T7], S7],
                                  f8: ISO[Option[T8], S8],
                                  f9: ISO[Option[T9], S9],
                                 ): XmlPickle[(S1, S2, S3, S4, S5, S6, S7, S8, S9)] = {
    Pickle(
      serializer = XmlSerializer.all(
        p1.serializer, p2.serializer, p3.serializer, p4.serializer, p5.serializer,
        p6.serializer, p7.serializer, p8.serializer, p9.serializer
      )(f1.undo, f2.undo, f3.undo, f4.undo, f5.undo,
        f6.undo, f7.undo, f8.undo, f9.undo
      ),
      parser = XmlParser.all(
        p1.parser, p2.parser, p3.parser, p4.parser, p5.parser,
        p6.parser, p7.parser, p8.parser, p9.parser
      )(f1.run, f2.run, f3.run, f4.run, f5.run,
        f6.run, f7.run, f8.run, f9.run
      )
    )
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8, T9, S9, T10, S10](p1: XmlPickle[T1],
                                            p2: XmlPickle[T2],
                                            p3: XmlPickle[T3],
                                            p4: XmlPickle[T4],
                                            p5: XmlPickle[T5],
                                            p6: XmlPickle[T6],
                                            p7: XmlPickle[T7],
                                            p8: XmlPickle[T8],
                                            p9: XmlPickle[T9],
                                            p10: XmlPickle[T10],
                                           )
                                           (f1: ISO[Option[T1], S1],
                                            f2: ISO[Option[T2], S2],
                                            f3: ISO[Option[T3], S3],
                                            f4: ISO[Option[T4], S4],
                                            f5: ISO[Option[T5], S5],
                                            f6: ISO[Option[T6], S6],
                                            f7: ISO[Option[T7], S7],
                                            f8: ISO[Option[T8], S8],
                                            f9: ISO[Option[T9], S9],
                                            f10: ISO[Option[T10], S10],
                                           ): XmlPickle[(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10)] = {
    Pickle(
      serializer = XmlSerializer.all(
        p1.serializer, p2.serializer, p3.serializer, p4.serializer, p5.serializer,
        p6.serializer, p7.serializer, p8.serializer, p9.serializer, p10.serializer,
      )(f1.undo, f2.undo, f3.undo, f4.undo, f5.undo,
        f6.undo, f7.undo, f8.undo, f9.undo, f10.undo
      ),
      parser = XmlParser.all(
        p1.parser, p2.parser, p3.parser, p4.parser, p5.parser,
        p6.parser, p7.parser, p8.parser, p9.parser, p10.parser
      )(f1.run, f2.run, f3.run, f4.run, f5.run,
        f6.run, f7.run, f8.run, f9.run, f10.run
      )
    )
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8, T9, S9, T10, S10,
  T11, S11](p1: XmlPickle[T1],
            p2: XmlPickle[T2],
            p3: XmlPickle[T3],
            p4: XmlPickle[T4],
            p5: XmlPickle[T5],
            p6: XmlPickle[T6],
            p7: XmlPickle[T7],
            p8: XmlPickle[T8],
            p9: XmlPickle[T9],
            p10: XmlPickle[T10],
            p11: XmlPickle[T11],
           )
           (f1: ISO[Option[T1], S1],
            f2: ISO[Option[T2], S2],
            f3: ISO[Option[T3], S3],
            f4: ISO[Option[T4], S4],
            f5: ISO[Option[T5], S5],
            f6: ISO[Option[T6], S6],
            f7: ISO[Option[T7], S7],
            f8: ISO[Option[T8], S8],
            f9: ISO[Option[T9], S9],
            f10: ISO[Option[T10], S10],
            f11: ISO[Option[T11], S11],
           ): XmlPickle[(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11)] = {
    Pickle(
      serializer = XmlSerializer.all(
        p1.serializer, p2.serializer, p3.serializer, p4.serializer, p5.serializer,
        p6.serializer, p7.serializer, p8.serializer, p9.serializer, p10.serializer,
        p11.serializer
      )(f1.undo, f2.undo, f3.undo, f4.undo, f5.undo,
        f6.undo, f7.undo, f8.undo, f9.undo, f10.undo,
        f11.undo
      ),
      parser = XmlParser.all(
        p1.parser, p2.parser, p3.parser, p4.parser, p5.parser,
        p6.parser, p7.parser, p8.parser, p9.parser, p10.parser,
        p11.parser
      )(f1.run, f2.run, f3.run, f4.run, f5.run,
        f6.run, f7.run, f8.run, f9.run, f10.run,
        f11.run
      )
    )
  }

  def all[T1, S1, T2, S2, T3, S3, T4, S4, T5, S5,
  T6, S6, T7, S7, T8, S8, T9, S9, T10, S10,
  T11, S11, T12, S12](p1: XmlPickle[T1],
                      p2: XmlPickle[T2],
                      p3: XmlPickle[T3],
                      p4: XmlPickle[T4],
                      p5: XmlPickle[T5],
                      p6: XmlPickle[T6],
                      p7: XmlPickle[T7],
                      p8: XmlPickle[T8],
                      p9: XmlPickle[T9],
                      p10: XmlPickle[T10],
                      p11: XmlPickle[T11],
                      p12: XmlPickle[T12],
                     )
                     (f1: ISO[Option[T1], S1],
                      f2: ISO[Option[T2], S2],
                      f3: ISO[Option[T3], S3],
                      f4: ISO[Option[T4], S4],
                      f5: ISO[Option[T5], S5],
                      f6: ISO[Option[T6], S6],
                      f7: ISO[Option[T7], S7],
                      f8: ISO[Option[T8], S8],
                      f9: ISO[Option[T9], S9],
                      f10: ISO[Option[T10], S10],
                      f11: ISO[Option[T11], S11],
                      f12: ISO[Option[T12], S12],
                     ): XmlPickle[(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12)] = {
    Pickle(
      serializer = XmlSerializer.all(
        p1.serializer, p2.serializer, p3.serializer, p4.serializer, p5.serializer,
        p6.serializer, p7.serializer, p8.serializer, p9.serializer, p10.serializer,
        p11.serializer, p12.serializer
      )(f1.undo, f2.undo, f3.undo, f4.undo, f5.undo,
        f6.undo, f7.undo, f8.undo, f9.undo, f10.undo,
        f11.undo, f12.undo
      ),
      parser = XmlParser.all(
        p1.parser, p2.parser, p3.parser, p4.parser, p5.parser,
        p6.parser, p7.parser, p8.parser, p9.parser, p10.parser,
        p11.parser, p12.parser
      )(f1.run, f2.run, f3.run, f4.run, f5.run,
        f6.run, f7.run, f8.run, f9.run, f10.run,
        f11.run, f12.run
      )
    )
  }

  def optional[X]: ISO[Option[X], Option[X]] = ISO(XmlParser.optional, XmlSerializer.optional)

  def mandatory[X]: ISO[Option[X], X] = ISO(XmlParser.mandatory, XmlSerializer.mandatory)
}
