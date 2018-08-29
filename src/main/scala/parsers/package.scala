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
  def succeed[A](v: A): Parser[A] = fail

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
  def fail: Parser[Nothing] = throw new Exception("not implemented")

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
  def anyChar: Parser[Char] = fail
}
