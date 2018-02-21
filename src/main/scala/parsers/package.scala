package object parsers {

  /** A parser of type `A` is a function that takes an input string and produces an optional result which is a pair of a result value of type `A` and an output string.
    *
    * @tparam A type of the result value
    */
  type Parser[+A] = String => Option[(A, String)]

  // Basic parsers

  /** The parser `succeed` always succeeds with the result value `v`.
    *
    * Usage example:
    *
    * {{{
    *   succeed(42)("input_string") == Some((42, "input_string"))
    * }}}
    *
    * @param v a value of type `A`
    * @tparam A type of the result value
    * @return a parser of type `A`
    */
  def succeed[A](v: A): Parser[A] = input => Some((v, input))

  /** The parser `fail` always fails, regardless of the contents of the input string.
    *
    * Usage example:
    *
    * {{{
    *   fail("input_string") == None
    * }}}
    *
    * @return an empty parser
    */
  def fail: Parser[Nothing] = _ => None

  /** The parser `anyChar` fails if the input string is empty or otherwise succeeds with the first character of the input string.
    *
    * Usage example:
    *
    * {{{
    *   anyChar("input_string") == Some(('i', "nput_string"))
    * }}}
    *
    * @return a parser of type `Char`
    */
  def anyChar: Parser[Char] = input => input.headOption.map((_, input.tail))

  // Sequencing and choice

  implicit class ParserCombinators[A](val p: Parser[A]) extends AnyVal {

    /** Sequencing parsers.
      *
      * The parser `p.flatMap(f)` fails if `p` fails.
      * Otherwise `f` will be applied to the result of `p` to create a second parser which will be applied to the remaining unconsumed input.
      *
      * @param f a function that creates a parser from a value of type `A`
      * @tparam B type of the resulting parser
      * @return a parser of type `B`
      */
    def flatMap[B](f: A => Parser[B]): Parser[B] =
      input =>
        p(input) match {
          case None           => fail(input)
          case Some((v, out)) => f(v)(out)
      }

    /** Returns a parser of type `B` by applying the function `f` to the inner value of this parser.
      *
      * @param f a function to apply
      * @tparam B the type of the resulting parser
      * @return a parser of type `B`
      */
    def map[B](f: A => B): Parser[B] = p.flatMap(v => succeed(f(v)))

    /** First is parser is applied. If it fails the a second parser will be applied.
      *
      * @param other a parser which will be applied if this parser fails
      * @return a parser of type `A`
      */
    def +++(other: Parser[A]): Parser[A] =
      input => p(input).orElse(other(input))
  }

  // Derived primitives

  /** Returns a parser for a single character that satisfies a given predicate `p`
    *
    * @param p the predicate
    * @return a Parser of type `Char`
    */
  def sat(p: Char => Boolean): Parser[Char] = ???

  /** Parses a single digit.
    *
    * Usage example:
    *
    * {{{
    *   digit("123") == Some(('1', "23"))
    *
    *   digit("abc") == None
    * }}}
    *
    */
  val digit: Parser[Char] = ???

  /** Parses a lower case character.
    *
    * Usage example:
    *
    * {{{
    *   lower("abc") == Some(('a', "bc"))
    *
    *   lower("ABC") == None
    * }}}
    *
    */
  val lower: Parser[Char] = ???

  /** Parses an upper case character.
    *
    * Usage example:
    *
    * {{{
    *   upper("ABC") == Some(('A', "BC"))
    *
    *   upper("abc") == None
    * }}}
    *
    */
  val upper: Parser[Char] = ???

  /** Parses any letter.
    *
    * Usage example:
    *
    * {{{
    *   letter("abc") == Some(('a', "bc"))
    *
    *   letter("123") == None
    * }}}
    *
    */
  val letter: Parser[Char] = ???

  /** Parses any letter or digit.
    *
    * Usage example:
    *
    * {{{
    *   alphaNum("abc") == Some(('a', "bc"))
    *
    *   alphaNum("123") == Some(('1', "23"))
    *
    *   alphaNum(":abc") == None
    * }}}
    *
    * @return a parser of type `Char`
    */
  val alphaNum: Parser[Char] = ???

  /** Parses a specific character.
    *
    * Usage example:
    *
    * {{{
    *   char('a')("abc") == Some(('a', "bc"))
    *
    *   char('z')("abc") == None
    * }}}
    *
    * @param c a specific character
    * @return a parser of type `Char`
    */
  def char(c: Char): Parser[Char] = ???

  /** Parses a specific string.
    *
    * Usage example:
    *
    * {{{
    *   string("foo")("foobar") == Some(("foo", "bar"))
    *
    *   string("foo")("abc") == None
    * }}}
    *
    *
    * @param str a specific string
    * @return a parser of type `String`
    */
  def string(str: String): Parser[String] = ???

  /** Applies a parser zero to many times until it fails and combines the results in a list.
    *
    * Usage example:
    *
    * {{{
    *   many(digit)("123abc") == Some((List('1', '2', '3'), "abc"))
    *
    *   many(digit)("abc) == Some((Nil, "abc"))
    * }}}
    *
    * @param p a parser to apply many times
    * @tparam A the type of the parser
    * @return a parsers of type `List[A]`
    */
  def many[A](p: Parser[A]): Parser[List[A]] = ???

  /** Applies a parser at least once to many times until it fails and combines the results in a list.
    *
    * Usage example:
    *
    * {{{
    *   many1(digit)("123abc") == Some((List('1', '2', '3'), "abc"))
    *
    *   many1(digit)("abc) == None
    * }}}
    *
    * @param p a parser to apply one to many times
    * @tparam A the type of the parser
    * @return a parser of type `List[A]`
    */
  def many1[A](p: Parser[A]): Parser[List[A]] = ???

  /** Parses at least one or more digits and converts them to a natural number of type `Int`.
    *
    * {{{
    *   nat("123foo") == Some((123, "foo"))
    *
    *   nat("abc") == None
    * }}}
    */
  val nat: Parser[Int] = ???

  /** Parses zero or more spaces, tabs or newline characters.
    *
    * {{{
    *   space(" \t abc") == Some(((), "abc"))
    *
    *   space("abc") == Some((), "abc"))
    * }}}
    *
    */
  val space: Parser[Unit] = ???
}
