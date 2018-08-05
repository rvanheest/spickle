package com.github.rvanheest.spickle.parser

import scala.util.{ Failure, Success, Try }

class Parser[S, A](private[parser] val parser: S => (Try[A], S)) {

  /**
   * Parse a particular input with this parser. If the parsing is successful, the tuple in the output
   * will contain the resulting object structure, as well as the remainder of the input that was left
   * unparsed (due to the parser finishing successful before all input was consumed).
   * If parsing fails, the tuple in the output will contain the `Throwable` that caused the parser
   * to fail, as well as the remainder of the input that was left unparsed.
   *
   * @param input the input to be parsed
   * @return both the result of parsing the input as well as the remainder of the input that was
   *         left unparsed.
   */
  def parse(input: S): (Try[A], S) = parser(input)

  /**
   * Parse a particular input with this parser and return only the result. This method discards the
   * remainder of the input that was left unparsed. If the parsing is successful, the output will
   * contain the resulting object structure; if the parsing fails, it will contain the `Throwable`
   * that caused the parser to fail.
   *
   * @param input the input to be parsed
   * @return the result of parsing the input
   */
  def eval(input: S): Try[A] = {
    val (result, _) = parse(input)
    result
  }

  /**
   * Parse a particular input with this parser and only return the remainder of the input that was
   * left unparsed (due to the parser finishing successful before all input was consumed).
   *
   * @param input the input to be parsed
   * @return the remainder of the input that was left unparsed
   */
  def execute(input: S): S = {
    val (_, remainder) = parse(input)
    remainder
  }

  /**
   * Uses this parser until it fails. If it fails, the `other` parser is used with the same input.
   * If this parser succeeds, the `other` parser is left unused.
   *
   * @param other the parser used as an alternative in the case this parser fails
   * @tparam B the result type of the `other` parser; being a supertype of `A`
   * @return either this parser if it succeeds, or `other` parser otherwise
   */
  def orElse[C, B](other: => Parser[S, B])(implicit ev: B <:< C, ev2: A <:< C): Parser[S, C] = {
    Parser(st => parse(st) match {
      case (Success(a), rest) => (Success(a), rest)
      case (Failure(_), _) => other.parse(st) match {
        case (Success(b), rest) => (Success(b), rest)
        case (Failure(e), rest) => (Failure(e), rest)
      }
    })
  }

  /**
   * Uses this parser until it fails. If it fails, the `other` parser is used with the same input.
   * If this parser succeeds, the `other` parser is left unused.
   *
   * Symbolic equivalent of [[Parser.orElse]]
   *
   * @param other the parser used as an alternative in the case this parser fails
   * @tparam B the result type of the `other` parser; being a supertype of `A`
   * @return either this parser if it succeeds, or `other` parser otherwise
   */
  def <|>[C >: A, B](other: => Parser[S, B])(implicit ev: B <:< C): Parser[S, C] = this.orElse(other)

  /**
   * Convert each token that is parsed with this parser using function `f`.
   *
   * @param f the function to convert the parsed token
   * @tparam B the result type of the conversion
   * @return a new parser that converts each token after being parsed
   */
  def map[B](f: A => B): Parser[S, B] = {
    Parser(st => {
      parse(st) match {
        case (Success(a), st2) => (Try(f(a)), st2)
        case (Failure(e), st2) => (Failure(e), st2)
      }
    })
  }

  /**
   * Replace every parsed token with the value `b`.
   *
   * @param b the value with which every parsed token is replaced
   * @tparam B the type of `b`
   * @return a new parser than replaces every token with a value after being parsed
   */
  def as[B](b: => B): Parser[S, B] = this.map(_ => b)

  /**
   * Replaces every parsed token with a `Unit`.
   *
   * @return a new parser than replaces every token with a `Unit` after being parsed
   */
  def void: Parser[S, Unit] = this.as(())

  /**
   * Combine this parser with a new parser that is generated based on a parsed token.
   * The new parser will continue parsing the next part of the yet unparsed input.
   *
   * @param f the function that generates the new parser
   * @tparam B the type of token that is parsed by the newly generated parser
   * @return a new parser that first parses a token and then generates a new parser, based on that
   *         token, that consumes (a part of) the remainder of the input
   */
  def flatMap[B](f: A => Parser[S, B]): Parser[S, B] = {
    Parser(st => {
      parse(st) match {
        case (Success(a), st2) => f(a).parse(st2)
        case (Failure(e), st2) => (Failure(e), st2)
      }
    })
  }

  /**
   * Combine this parser with a new parser that is generated based on a parsed token.
   * The new parser will continue parsing the next part of the yet unparsed input.
   *
   * Symbolic equivalent of [[Parser.flatMap]]
   *
   * @param f the function that generates the new parser
   * @tparam B the type of token that is parsed by the newly generated parser
   * @return a new parser that first parses a token and then generates a new parser, based on that
   *         token, that consumes (a part of) the remainder of the input
   */
  def >>=[B](f: A => Parser[S, B]): Parser[S, B] = this.flatMap(f)

  /**
   * Transforms a pair of parsed token and remaining input into a new pair of token and input.
   * This can also be viewed as changing the current state of the parser with a new state.
   *
   * @param f the function to transform the parser's current state to a new state
   * @tparam B the type of token parsed by the new parser
   * @return a new parser with the new state
   */
  def transform[B](f: (A, S) => (Try[B], S)): Parser[S, B] = {
    Parser(st => {
      parse(st) match {
        case (Success(a), st2) => f(a, st2)
        case (Failure(e), st2) => (Failure(e), st2)
      }
    })
  }

  /**
   * Combine two parsers as in [[Parser.flatMap]] while discarding the token from this parser.
   *
   * @param other the parser that is used to parse the second token (which is NOT discarded)
   * @tparam B the type of the token that is parsed second
   * @return a new parser that first parses a token which is discarded and then parses a next token
   */
  def >>[B](other: => Parser[S, B]): Parser[S, B] = this >>= (_ => other)

  /**
   * Combine two parsers as in [[Parser.flatMap]] while discarding the token from `other`.
   *
   * @param other the parser that is used to parse the second token (which is discarded)
   * @tparam B the type of the token that is parsed second
   * @return a new parser that first parses a token and then parses a next token which is discarded
   */
  def <<[B](other: => Parser[S, B]): Parser[S, A] = this >>= (x => other >> Parser.from(x))

  /**
   * Checks whether the parsed token satisfies a predicate. If it doesn't, the parse fails with
   * a default text.
   *
   * @param predicate the predicate against which a parsed token is validated
   * @return a new parser that parses a token and then validates it against `predicate`
   */
  def satisfy(predicate: A => Boolean): Parser[S, A] = {
    this.satisfy(predicate, a => s"input '$a' did not satisfy predicate")
  }

  /**
   * Checks whether the parsed token satisfies a predicate. If it doesn't, the parse fails with
   * the message constructed by `errMsg`.
   *
   * @param predicate the predicate against which a parsed token is validated
   * @param errMsg    generates the error message to be displayed with a failing parse
   * @return a new parser that parses a token and then validates it against `predicate`
   */
  def satisfy(predicate: A => Boolean, errMsg: A => String): Parser[S, A] = {
    this >>= (a => if (predicate(a)) Parser.from(a)
                   else Parser.failure(ParserFailedException(errMsg(a))))
  }

  /**
   * Checks whether the parsed token satisfies a predicate. If it doesn't, the parse fails with
   * a default text.
   *
   * Note that this method is equal to [[Parser.satisfy]], but is provided to support for-comprehensions
   * with `if`-expressions.
   *
   * @param predicate the predicate against which a parsed token is validated
   * @return a new parser that parses a token and then validates it against `predicate`
   */
  def filter(predicate: A => Boolean): Parser[S, A] = this.satisfy(predicate)

  /**
   * Creates a parser that only succeeds when none of the given tokens in `as` is parsed.
   *
   * @param as the tokens that are not allowed
   * @return a new parser that checks if none of the given tokens are being found while parsing
   */
  def noneOf(as: Seq[A]): Parser[S, A] = {
    this.satisfy(!as.contains(_), a => s"input '$a' is contained in ${ as.mkString("[", ", ", "]") }")
  }

  /**
   * Marks this parser as being optional with regard to the input. The input this parser can
   * parse may or may not exist. If this parser fails, the output of `.maybe` will be [[Option.empty]].
   *
   * @return a new parser that makes the token to be parsed by this parser optional
   */
  def maybe: Parser[S, Option[A]] = this.map(Option(_)) <|> Parser.from(Option.empty)

  /**
   * Repeats parsing with this parser for as long as it succeeds in parsing the next token.
   * This can be 0 or more times.
   *
   * @return a new parser that can parse a certain kind of token 0 or more times
   */
  def many: Parser[S, Seq[A]] = this.atLeastOnce <|> Parser.from(Nil)

  /**
   * Repeats parsing with this parser for as long as it succeeds in parsing the next token, but at
   * leat once.
   *
   * @return a new parser that can parse a certain token 1 or more times
   */
  def atLeastOnce: Parser[S, Seq[A]] = {
    for {
      x <- this
      xs <- this.many
    } yield x +: xs
  }

  /**
   * Repeats parsing with this parser until either it fails parsing the next token or the given
   * predicate fails.
   *
   * Note: `parser.takeUntil(p) === parser.takeWhile(!p)`
   *
   * @param predicate the predicate that controls for how long the parser will continue parsing
   *                  the next token
   * @return a new parser that can parse many consecutive tokens while the predicate remains `true`
   */
  def takeUntil(predicate: A => Boolean): Parser[S, Seq[A]] = this.takeWhile(!predicate(_))

  /**
   * Repeats parsing with this parser until it fails parsing the next token or as long as the
   * predicate succeeds.
   *
   * Note: `parser.takeWhile(p) === parser.takeUntil(!p)`
   *
   * @param predicate the predicate that controls for how long the parser will continue parsing
   *                  the next token
   * @return a new parser that can parse many consecutive tokens while the predicate remains `true`
   */
  def takeWhile(predicate: A => Boolean): Parser[S, Seq[A]] = this.satisfy(predicate).many

  /**
   * Repeats parsing with this parser for as long as it succeeds in parsing the next token.
   * This can be 0 or more times. While doing that, it removes the separation token(s) parsed by
   * the `separation` parser.
   *
   * @param separation the parser that parses the separation token(s) between the token(s) parsed
   *                   by this parser
   * @tparam Sep the type of token parsed by the `separation` parser
   * @return a new parser that repeats parsing tokens with this parser, while removing intermediate
   *         separation tokens using the `separation` parser
   */
  def separatedBy[Sep](separation: Parser[S, Sep]): Parser[S, Seq[A]] = {
    this.separatedBy1(separation) <|> Parser.from(Nil)
  }

  /**
   * Repeats parsing with this parser for as long as it succeeds in parsing the next token.
   * This can be 1 or more times. While doing that, it removes the separation token(s) parsed by
   * the `separation` parser.
   *
   * @param separation the parser that parses the separation token(s) between the token(s) parsed
   *                   by this parser
   * @tparam Sep the type of token parsed by the `separation` parser
   * @return a new parser that repeats parsing tokens with this parser, while removing intermediate
   *         separation tokens using the `separation` parser
   */
  def separatedBy1[Sep](separation: Parser[S, Sep]): Parser[S, Seq[A]] = {
    for {
      x <- this
      xs <- (separation >> this).many
    } yield x +: xs
  }

  /**
   * Repeatedly parse the next token with this parser and discarding the result(s).
   *
   * @return a new parser that repeatedly discards the results of the parse
   */
  def skipMany: Parser[S, Unit] = this >> this.skipMany <|> Parser.from(())
}

object Parser {

  /**
   * Wraps a parser function in a `Parser` object.
   *
   * @param parser the function to be wrapped
   * @tparam S the type of the tokens to be parsed
   * @tparam A the type of a token after it's being parsed
   * @return a new `Parser`
   */
  def apply[S, A](parser: S => (Try[A], S)) = new Parser(parser)

  /**
   * Create a `Parser` that does not consume any token, but sets the consumed/parsed token to be
   * the given token
   *
   * @param a the token to be consumed
   * @tparam S the type of the tokens to be parsed
   * @tparam A the type of a token after it's being parsed
   * @return a new `Parser` with the input as the parsed token
   */
  def from[S, A](a: A): Parser[S, A] = Parser((Success(a), _))

  /**
   * Create a `Parser` that always fails with a [[java.util.NoSuchElementException]].
   *
   * @tparam S the type of the tokens to be parsed
   * @tparam A the type of a token after it's being parsed
   * @return a new `Parser` that immediately fails
   */
  def empty[S, A]: Parser[S, A] = Parser.failure(new NoSuchElementException("empty parser"))

  /**
   * Create a `Parser` that immediately fails with the given [[java.lang.Throwable]].
   *
   * @param e the [[java.lang.Throwable]] to be returned in failing
   * @tparam S the type of the tokens to be parsed
   * @tparam A the type of a token after it's being parsed
   * @return a new `Parser` that immediately fails with the given error
   */
  def failure[S, A](e: Throwable): Parser[S, A] = Parser((Failure(e), _))

  /**
   * Create a `Parser` that prints a debug line containing the `Parser`'s current input and quites
   * the program afterwards with an error.
   *
   * @param pos an identifier given by the user to distinguish between multiple debug lines
   * @tparam S the type of the tokens to be parsed
   * @return a new `Parser` that fails after having printed a debug statement.
   */
  def debugAndFail[S](pos: String): Parser[S, Nothing] = {
    Parser(xs => sys.error(s"you hit a debug statement at $pos: $xs"))
  }

  /**
   * Create a `Parser` that prints a debug line containing the `Parser`'s current input and
   * proceeds wih the program without consuming a token.
   *
   * @param pos an identifier given by the user to distinguish between multiple debug lines
   * @param debugger the type of debugger to use, [[println]] by default
   * @tparam S the type of the tokens to be parsed
   * @return a new `Parser` that continues parsing after having printed a debug statement.
   */
  def debugAndContinue[S](pos: String)(implicit debugger: String => Unit = println): Parser[S, Unit] = {
    Parser(xs => {
      debugger(s"you hit a debug statement at $pos: $xs")
      (Success(()), xs)
    })
  }

  /**
   * Enriches the API for parsers that parse a token to a `String`. This class contains some
   * convenience methods that make programming a complex parser a little easier.
   *
   * @param parser the parser to be enriched
   * @tparam State the type of tokens to be parsed by the given parser
   */
  implicit class StringOperators[State](val parser: Parser[State, String]) extends AnyVal {

    /**
     * Convert the result of this parse to a `Byte`
     *
     * @return a new parser that converts the result of a parse to a `Byte`
     */
    def toByte: Parser[State, Byte] = parser.map(_.toByte)

    /**
     * Convert the result of this parse to a `Short`
     *
     * @return a new parser that converts the result of a parse to a `Short`
     */
    def toShort: Parser[State, Short] = parser.map(_.toShort)

    /**
     * Convert the result of this parse to a `Int`
     *
     * @return a new parser that converts the result of a parse to a `Int`
     */
    def toInt: Parser[State, Int] = parser.map(_.toInt)

    /**
     * Convert the result of this parse to a `Long`
     *
     * @return a new parser that converts the result of a parse to a `Long`
     */
    def toLong: Parser[State, Long] = parser.map(_.toLong)

    /**
     * Convert the result of this parse to a `Float`
     *
     * @return a new parser that converts the result of a parse to a `Float`
     */
    def toFloat: Parser[State, Float] = parser.map(_.toFloat)

    /**
     * Convert the result of this parse to a `Double`
     *
     * @return a new parser that converts the result of a parse to a `Double`
     */
    def toDouble: Parser[State, Double] = parser.map(_.toDouble)
  }
}
