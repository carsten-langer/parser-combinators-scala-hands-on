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
    def flatMap[B](f: A => Parser[B]): Parser[B] = fail

    /** Returns a parser of type `B` by applying the function `f` to the inner value of this parser.
      *
      * @param f a function to apply
      * @tparam B the type of the resulting parser
      * @return a parser of type `B`
      */
    def map[B](f: A => B): Parser[B] = fail

    /** First is parser is applied. If it fails the a second parser will be applied.
      *
      * @param other a parser which will be applied if this parser fails
      * @return a parser of type `A`
      */
    def +++(other: Parser[A]): Parser[A] = fail
  }

}
