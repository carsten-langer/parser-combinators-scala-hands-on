package object parsers {

  type Parser[+A] = String => Option[(A, String)]

  // Basic parsers

  def succeed[A](v: A): Parser[A] = ???

  def fail: Parser[Nothing] = ???

  def item: Parser[Char] = ???
}
