package parsers

object ArithmeticExpressions {

  /*
   grammer:

   expr ::= term (+ expr | \epsilon) \
   term ::= factor (* term | \epsilon) \
   factor ::= (expr) | nat \
   nat ::= 0 | 1 | 2 | \dots \
   */

  lazy val expr: Parser[Int] = ???

  lazy val term: Parser[Int] = ???

  lazy val factor: Parser[Int] = ???

  def eval(input: String): Either[String, Int] =
    expr(input) match {
      case Some((n, ""))  => Right(n)
      case Some((_, out)) => Left(s"unconsumed input: $out")
      case None           => Left("invalid input")
    }
}
