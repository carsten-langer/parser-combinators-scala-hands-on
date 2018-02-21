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

  def parse[A](p: Parser[A], input: String): Option[(A, String)] = p(input)

  implicit class Parse(val input: String) extends AnyVal {
    def parse[A](p: Parser[A]): Option[(A, String)] = parsers.parse(p, input)
  }

  // Sequencing and choice

  implicit class ParserCombinators[A](val p: Parser[A]) extends AnyVal {
    def flatMap[B](f: A => Parser[B]): Parser[B] =
      input =>
        input.parse(p) match {
          case None           => fail(input)
          case Some((v, out)) => out.parse(f(v))
        }

    def map[B](f: A => B): Parser[B] = p.flatMap(v => succeed(f(v)))

    def +++(other: Parser[A]): Parser[A] =
      input =>
        input.parse(p) match {
          case None             => input.parse(other)
          case result @ Some(_) => result
        }
  }

  // Derived primitives

  def sat(predicate: Char => Boolean): Parser[Char] = ???

  val digit: Parser[Char] = ???

  val lower: Parser[Char] = ???

  val upper: Parser[Char] = ???

  val letter: Parser[Char] = ???

  val alphaNum: Parser[Char] = ???

  def char(c: Char): Parser[Char] = ???

  def string(str: String): Parser[String] = ???

  def many[A](p: Parser[A]): Parser[List[A]] = ???

  def many1[A](p: Parser[A]): Parser[List[A]] = ???

  val ident: Parser[String] = ???

  val nat: Parser[Int] = ???

  val space: Parser[Unit] = ???
}
