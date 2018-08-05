package com.github.rvanheest.spickle.pickle.xml

import com.github.rvanheest.spickle.MyGeneric
import com.github.rvanheest.spickle.parser.xml.AllParserBuilder
import com.github.rvanheest.spickle.pickle.Pickle
import com.github.rvanheest.spickle.pickle.xml.XmlPickle.XmlPickle
import com.github.rvanheest.spickle.serializer.xml.AllSerializerBuilder
import shapeless.{ ::, HList, HNil }

class AllPickleBuilder[MyHList <: HList] private[xml](private[xml] val aggregateParser: AllParserBuilder[MyHList],
                                                      private[xml] val aggregateSerializer: AllSerializerBuilder[MyHList]) {

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

  def build[T](gen: MyGeneric[T, MyHList]): XmlPickle[T] = {
    Pickle(
      parser = aggregateParser.build(gen),
      serializer = aggregateSerializer.build(gen)
    )
  }

  def seq[MyHList2 <: HList](f: MyHList2 => MyHList): AllPickleBuilderSeqBuilder[MyHList, MyHList2] = {
    new AllPickleBuilderSeqBuilder(this, f)
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

class AllPickleBuilderSeqBuilder[MyHList <: HList, MyHList2 <: HList](builder: AllPickleBuilder[MyHList], f: MyHList2 => MyHList) {
  def map(g: MyHList => MyHList2): AllPickleBuilder[MyHList2] = {
    new AllPickleBuilder[MyHList2](
      builder.aggregateParser.map(g),
      builder.aggregateSerializer.contramap(f)
    )
  }

  def flatMap(g: MyHList => AllPickleBuilder[MyHList2]): AllPickleBuilder[MyHList2] = {
    new AllPickleBuilder[MyHList2](
      builder.aggregateParser.flatMap(g(_).aggregateParser),
      builder.aggregateSerializer.contramapCombine(f, g(_).aggregateSerializer)
    )
  }
}
