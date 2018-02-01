package object parsers {

  type Parser[+A] = String => Option[(A, String)]

  // Basic parsers

  def succeed[A](v: A): Parser[A] = input => Some(v, input)

  def fail: Parser[Nothing] = _ => None

  def item: Parser[Char] =
    input =>
      if (input.nonEmpty) succeed(input.charAt(0))(input.substring(1))
      else fail(input)

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

}
