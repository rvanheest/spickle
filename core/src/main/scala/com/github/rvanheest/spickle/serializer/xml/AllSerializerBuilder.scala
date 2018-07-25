package com.github.rvanheest.spickle.serializer.xml

import com.github.rvanheest.spickle.serializer.xml.XmlSerializer.XmlSerializer
import shapeless.{ ::, HList, HNil }

class AllSerializerBuilder[MyHList <: HList] private(private val aggregate: XmlSerializer[MyHList]) {

  def andMandatory[T](serializer: XmlSerializer[T]): AllSerializerBuilder[T :: MyHList] = {
    new AllSerializerBuilder(
      aggregate.contramap[T :: MyHList] { case _ :: tail => tail }
        .combine(serializer.maybe.contramap[T :: MyHList] { case newItem :: _ => Option(newItem) })
    )
  }

  def andOptional[T](serializer: XmlSerializer[T]): AllSerializerBuilder[Option[T] :: MyHList] = {
    new AllSerializerBuilder(
      aggregate.contramap[Option[T] :: MyHList] { case _ :: tail => tail }
        .combine(serializer.maybe.contramap[Option[T] :: MyHList] { case newItem :: _ => newItem })
    )
  }

  def build: XmlSerializer[MyHList] = aggregate
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
