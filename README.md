spickle
=======
[![Build Status](https://travis-ci.com/rvanheest/spickle.svg?branch=master)](https://travis-ci.com/rvanheest/spickle)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/fa0990cb9bbf414ebf7cd94b60a93e19)](https://www.codacy.com/app/rvanheest/spickle?utm_source=github.com&utm_medium=referral&utm_content=rvanheest/spickle&utm_campaign=badger)
[![Codacy Badge](https://api.codacy.com/project/badge/Coverage/fa0990cb9bbf414ebf7cd94b60a93e19)](https://www.codacy.com/app/rvanheest/spickle?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=rvanheest/spickle&amp;utm_campaign=Badge_Coverage)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.rvanheest/spickle_2.12/badge.svg)](http://search.maven.org/#search%7Cga%7C1%7Ccom.github.rvanheest.spickle)

**spickle** is a library for serializing, parsing and pickling data in Scala.

Often, parsers and serializers are written separate from each other. This can quickly result in updating
the one, while forgetting the other. When no good (automated) testing is put in place on beforehand,
this may ultimately lead to bugs in production code.

This library offers support for implementing both parsers and serializers separately, but also combines
the two into one API, such that you get both parser and serializer with only one peace of code (also
known as a _pickle_ in [research](https://www.microsoft.com/en-us/research/wp-content/uploads/2004/01/picklercombinators.pdf)).

Complex parsers, serializers and picklers are constructed by starting with simple ones and combining
them using the many operators defined in this library. Simple building blocks are provided to start
off with. This library currently supports these building blocks for parsing, serializing and pickling
Strings and XML structures.


Binaries
--------

Binaries and dependency information for Maven, Ivy, Gradle and others can be found at [http://search.maven.org](http://search.maven.org/#search%7Cga%7C1%7Ccom.github.rvanheest.spickle).

Example for Gradle

```groovy
compile 'com.github.rvanheest:spickle_2.12:x.y.z'
```

and for Maven:

```xml
<dependency>
    <groupId>com.github.rvanheest</groupId>
    <artifactId>spickle_2.12</artifactId>
    <version>x.y.z</version>
</dependency>
```

and for Ivy:
```xml
<dependency org="com.github.rvanheest" name="spickle_2.12" rev="x.y.z" />
```

and for SBT:
```sbtshell
libraryDependencies += "com.github.rvanheest" %% "spickle" % "x.y.z"
```


Build
-----

To build:

```
    $ git clone git@github.com:rvanheest/spickle.git
    $ cd spickle/
    $ mvn clean install
```


Bugs and Feedback
-----------------

For bugs, questions and discussion please use the [GitHub Issues](https://github.com/rvanheest/spickle/issues).


License
-------

**spickle** is available under the Apache 2 License. Please see the [license](LICENSE) for more information.


Examples
--------

### Parse XML
To parse an XML structure like

```xml
<person>
    <name>Jim Jones</name>
    <age>36</age>
    <favoriteNumber>1</favoriteNumber>
    <favoriteNumber>3</favoriteNumber>
    <favoriteNumber>5</favoriteNumber>
    <favoriteNumber>7</favoriteNumber>
    <favoriteNumber>11</favoriteNumber>
</person>
```

we define a `Parser` like:

```scala
import com.github.rvanheest.spickle.parser.xml.XmlParser._
import scala.util.Success

case class Person(name: String, age: Int, favoriteNumbers: Seq[Int])

def parsePerson: XmlParser[Person] = {
  branchNode("person") {
    for {
      name <- stringNode("name")
      age <- stringNode("age").toInt
      favoriteNumbers <- stringNode("favoriteNumber").toInt.many
    } yield Person(name, age, favoriteNumbers)
  }
}

val (Success(person), remainingXml) = parsePerson.parse(xml)
// person: Person("Jim Jones", 36, Seq(1, 3, 5, 7, 11))
// remainingXml should be empty

// to only get the parsed object
val Success(person) = parsePerson.eval(xml)

// to only get the remaining xml
val remainingXml = parsePerson.execute(xml)

```

### Parse and serialize XML
When we also require the serializer, such that the object gets written back to XML, the `Pickle` can
be used instead. We define:

```scala
import com.github.rvanheest.spickle.pickle.xml.XmlPickle._
import scala.util.Success

case class Person(name: String, age: Int, favoriteNumbers: Seq[Int])

def picklePerson: XmlPickle[Person] = {
  branchNode("person") {
    for {
      name <- stringNode("name").seq[Person](_.name)
      age <- stringNode("age").toInt.seq[Person](_.age)
      favoriteNumbers <- stringNode("favoriteNumber").toInt.many.seq[Person](_.favoriteNumbers)
    } yield Person(name, age, favoriteNumbers)
  }
}

val (Success(person), remainingXml) = picklePerson.parse(xml)
// person: Person("Jim Jones", 36, Seq(1, 3, 5, 7, 11))
// remainingXml should be empty

val Success(Seq(serializedPersonXml)) = picklePerson.serialize(person, Seq.empty)
// serializedPersonXml is equal to the original xml that we parsed
```

Notice that this `Pickle[Person]` looks almost the same as the `Parser[Person]` that was defined in
previous example. The only difference is the `.seq[Person](...)`, which is the part that is used by
the serializer to access the particular fields in the `Person` object. Because `stringNode("age").toInt`
and `stringNode("favoriteNumber").toInt.many` are already used by the serializer, there are no further
transformations required in `.seq[Person](...)` than providing the field accessors.

### More XML examples
For more examples on XML parsing, including attributes, namespaces, `<xs:all>`, etc. checkout the
[examples project](example/src/main/scala/com/github/rvanheest/spickle/example/xml).

### String parsing
In the example projects, an [ExpressionParser](example/src/main/scala/com/github/rvanheest/spickle/example/string/ExpressionParser.scala)
is defined, which takes an arithmetic expression as an input (like `"(2 * 3) + (2 + 7)"`) and
evaluates this expression (in the example, it returns `15`). Note that the syntax is equivalent to
that of the XML parser above, but with atomic parsers on Strings. 

Notice, however, that here the structure of the original expression is not preserved and hence it is
impossible to define a serializer or pickler for this example. 


Technical motivation
--------------------
As is well known, parsers can be expressed as a function `S -> (T, S)`, taking a state as an input
and transforming that to an object `T` and a remaining state. [Parser combinators](http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf)
can then be defined to combine these functions and construct complexer parsers from them.
Since a parser is a [monad](https://wiki.haskell.org/Monad), composition can be done using the
familiar `map` and `flatMap` operators. Therefore we can use Scala's `for`-comprehension to neatly 
define our parsers. From the monadic operators we can also more complex operators such as `maybe`,
`many`, `atLeastOnce`, `takeWhile`, etc. can be constructed. 

Taking the dual of the function above, results in a function (`(T, S) -> S`) that takes both an
object `T` and a current state, and converts the `T` into a new state `S` together with the current
state. This is typically called a serializer. This type does not define a monad, but can be viewed
as a [contravariant functor](http://hackage.haskell.org/package/contravariant-1.5/docs/Data-Functor-Contravariant.html),
and also as a [monoid](https://wiki.haskell.org/Monoid). Hence we can define operators like `contramap`
and `combine` to construct complex serializers such as `maybe`, `many`, `atLeastOnce`, `takeWhile`, etc.

Combining the parser and serializer together results in a [pickler](https://www.microsoft.com/en-us/research/wp-content/uploads/2004/01/picklercombinators.pdf).
Just like before, we aim to compose complex picklers out of simpler ones, which in turn are composed
out of simple parsers and serializers. With a little extra work to get over the covariant functor 
from the serializer, we can define the monadic operators for a pickler as well, leading once again
to more complex operators that are defined out of the box.

Next to combinators, this library defines specialized parsers, serializers and picklers for commonly
used datastructures. String parsing can be quite cumbersome and writing an interpreter for a string
if often not a trivial task. Also parsing and serializing XML can cost a lot of time and effort,
especially to ensure that `serialize(parse(xml)) == xml`. Especially when parsers and serializers
are written separately (for example using Scala's [XML library](https://github.com/scala/scala-xml)),
it isn't hard to write code where `serialize(parse(xml)) != xml`.
Because of these reasons, **spickle** defines basic building blocks for both Strings and XML. With
these, you can easily write string parsers and interpreters, as well as write simple parsers,
serializers and picklers for XML structures.


FAQ
---

> What does **spickle** stand for?

_**spickle** contains the word 'pickle', which is what this library is all about. Since the library
targets Scala as programming language, this became **spickle**._

> Why do we need a library like **spickle**? Isn't scala-xml good enough? Can't we do String parsing
with regular expressions?

_This library grew out of a frustration I had while converting a large (more than 5000 lines) XSD to
Scala objects and writing a parser using scala-xml. I found that in some cases, the xpath syntax did
not suffice and that I had to do magic in order to get the XML parsed correctly. The thought of also
having to write a serializer and maintain the XSD, the object structure, the parser and the serializer
made me question whether there was a better way of doing this. That's when I started looking into
picklers and in particular how they are implemented in other functional languages like Haskell.
While the basic idea was quite nice, I thought I could do better. Hence I set out to implement
a functional parser and serializer and combine the two into a pickler. While studying parser combinators,
I found that the same concept could also be used for String parsing, and many more applications._

> Why only specialize for XML and String? Why not have specializations for JSON as well, for example?

_The main reason why **spickle** only supports XML and String is because that's what I set out with.
I simply haven't got any time yet to add other data types. Besides, the reason I haven't added JSON
as a third specialization (yet) is that Scala's de facto standard for JSON parsing/serialization
([json4s](http://json4s.org/)) basically already defines a pickler: once you have your object model
conform to the JSON, you can use [`read` and `write`](http://json4s.org/#serialization) to convert
between object model and JSON._

> I have another common data type. Can I use **spickle** for that?

_Sure! I would start with the parser, after which the serializer and pickler are relatively simple
to write. Think of atomic units in your data type and write a parser and serializer for those first.
Then use these atomics to construct complexer units of your data type, until you have parsers for the
whole data type. Look at the examples for strings and XML for inspiration._

_Besides, always feel free to contribute to **spickle**! If you think other people might benefit from
your picklers as well, send a pull request._
