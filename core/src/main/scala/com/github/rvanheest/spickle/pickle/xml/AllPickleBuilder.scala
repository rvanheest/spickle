package com.github.rvanheest.spickle.pickle.xml

import com.github.rvanheest.spickle.parser.xml.AllParserBuilder
import com.github.rvanheest.spickle.pickle.Pickle
import com.github.rvanheest.spickle.pickle.xml.XmlPickle.XmlPickle
import com.github.rvanheest.spickle.serializer.xml.AllSerializerBuilder
import shapeless.{ ::, HList, HNil }

class AllPickleBuilder[MyHList <: HList] private(private val aggregateParser: AllParserBuilder[MyHList],
                                                 private val aggregateSerializer: AllSerializerBuilder[MyHList]) {

  def andMandatory[T](pickle: XmlPickle[T]): AllPickleBuilder[T :: MyHList] = {
    new AllPickleBuilder(
      aggregateParser.andMandatory(pickle.parser),
      aggregateSerializer.andMandatory(pickle.serializer)
    )
  }

  def andOptional[T](pickle: XmlPickle[T]): AllPickleBuilder[Option[T] :: MyHList] = {
    new AllPickleBuilder(
      aggregateParser.andOptional(pickle.parser),
      aggregateSerializer.andOptional(pickle.serializer)
    )
  }

  def build: XmlPickle[MyHList] = {
    Pickle(
      parser = aggregateParser.build,
      serializer = aggregateSerializer.build
    )
  }
}

object AllPickleBuilder {

  def fromMandatory[T](pickle: XmlPickle[T]): AllPickleBuilder[T :: HNil] = {
    new AllPickleBuilder(
      AllParserBuilder.fromMandatory(pickle.parser),
      AllSerializerBuilder.fromMandatory(pickle.serializer)
    )
  }

  def fromOptional[T](pickle: XmlPickle[T]): AllPickleBuilder[Option[T] :: HNil] = {
    new AllPickleBuilder(
      AllParserBuilder.fromOptional(pickle.parser),
      AllSerializerBuilder.fromOptional(pickle.serializer)
    )
  }
}
