package parsers

object ListParsers {

  /** A parser for lists of natural numbers that ignores spaces around tokens.
    *
    * {{{
    *   nonEmptyIntList("[1, 2, 3, 4] abc") == Some((List(1, 2, 3, 4), "abc"))
    *
    *   nonEmptyIntList("[1, 2,] abc") == None
    * }}}
    *
    */
  val nonEmptyIntList: Parser[List[Int]] =
    for {
      _ <- symbol("[")
      n <- natural
      ns <- many(for {
        _ <- symbol(",")
        n1 <- natural
      } yield n1)
      _ <- symbol("]")
    } yield n :: ns}
