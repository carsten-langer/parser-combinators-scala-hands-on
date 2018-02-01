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

  // Derived primitives

  def sat(predicate: Char => Boolean): Parser[Char] =
    for {
      c <- item
      r <- if (predicate(c)) succeed(c) else fail
    } yield r

  val digit: Parser[Char] = sat(_.isDigit)

  val lower: Parser[Char] = sat(_.isLower)

  val upper: Parser[Char] = sat(_.isUpper)

  val letter: Parser[Char] = sat(_.isLetter)

  val alphaNum: Parser[Char] = sat(_.isLetterOrDigit)

  def char(c: Char): Parser[Char] = sat(_ == c)

  def string(str: String): Parser[String] =
    if (str.isEmpty) succeed("")
    else
      for {
        _ <- char(str.charAt(0))
        _ <- string(str.substring(1))
      } yield str

  def many[A](p: Parser[A]): Parser[List[A]] =
    many1(p) +++ succeed(Nil)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    for {
      head <- p
      tail <- many(p)
    } yield head :: tail

  val ident: Parser[String] =
    for {
      x <- lower
      xs <- many(alphaNum)
    } yield (x :: xs).mkString

  val nat: Parser[Int] =
    for {
      xs <- many1(digit)
    } yield xs.mkString.toInt

  val space: Parser[Unit] =
    for {
      _ <- many(sat(_.isWhitespace))
    } yield ()

  // Handling spaces

  def token[A](p: Parser[A]): Parser[A] = ???

  val identifier: Parser[String] = ???

  val natural: Parser[Int] = ???

  def symbol(s: String): Parser[String] = ???
}
