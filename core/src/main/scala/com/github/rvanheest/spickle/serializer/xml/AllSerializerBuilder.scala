package com.github.rvanheest.spickle.serializer.xml

import com.github.rvanheest.spickle.MyGeneric
import com.github.rvanheest.spickle.serializer.xml.XmlSerializer.XmlSerializer
import shapeless.{ ::, HList, HNil }

class AllSerializerBuilder[MyHList <: HList] private(private val aggregate: XmlSerializer[MyHList]) {

  def andMandatory[T](serializer: XmlSerializer[T]): AllSerializerBuilder[T :: MyHList] = {
    new AllSerializerBuilder(
      aggregate.contramap[T :: MyHList] { case _ :: tail => tail }
        .combine(serializer.maybe.contramap { case newItem :: _ => Option(newItem) })
    )
  }

  def andOptional[T](serializer: XmlSerializer[T]): AllSerializerBuilder[Option[T] :: MyHList] = {
    new AllSerializerBuilder(
      aggregate.contramap[Option[T] :: MyHList] { case _ :: tail => tail }
        .combine(serializer.maybe.contramap { case newItem :: _ => newItem })
    )
  }

  def build: XmlSerializer[MyHList] = aggregate

  def build[T](gen: MyGeneric[T, MyHList]): XmlSerializer[T] = {
    aggregate contramap gen.to
  }

  def contramap[MyHList2 <: HList](f: MyHList2 => MyHList): AllSerializerBuilder[MyHList2] = {
    new AllSerializerBuilder(aggregate contramap f)
  }

  def combine(that: AllSerializerBuilder[MyHList]): AllSerializerBuilder[MyHList] = {
    new AllSerializerBuilder(aggregate combine that.aggregate)
  }

  def contramapCombine[MyHList2 <: HList](f: MyHList2 => MyHList,
                                          g: MyHList => AllSerializerBuilder[MyHList2]): AllSerializerBuilder[MyHList2] = {
    new AllSerializerBuilder[MyHList2](aggregate.contramapCombine(f, g(_).aggregate))
  }
}

object AllSerializerBuilder {

  def fromMandatory[T](serializer: XmlSerializer[T]): AllSerializerBuilder[T :: HNil] = {
    new AllSerializerBuilder(
      serializer.maybe.contramap[T :: HNil] { case newItem :: _ => Option(newItem) }
    )
  }

  def fromOptional[T](serializer: XmlSerializer[T]): AllSerializerBuilder[Option[T] :: HNil] = {
    new AllSerializerBuilder(
      serializer.maybe.contramap[Option[T] :: HNil] { case newItem :: _ => newItem }
    )
  }
}
