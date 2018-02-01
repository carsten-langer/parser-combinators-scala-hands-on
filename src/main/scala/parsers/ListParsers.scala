package parsers

object ListParsers {
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
