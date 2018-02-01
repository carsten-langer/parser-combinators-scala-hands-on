package parsers

import org.scalatest._
import org.scalatest.prop._
import parsers.ListParsers._

class ListParsersTests extends PropSpec with PropertyChecks with Matchers {
  val validLists =
    Table(
      ("input", "int list"),
      ("[1,5,3,5]", List(1, 5, 3, 5)),
      ("  [   4 ,    5,6,7  ,  3 ,   9]", List(4, 5, 6, 7, 3, 9)),
      ("[1]", List(1))
    )

  val invalidLists =
    Table(
      "input",
      "1,5,3,5]",
      "[1,5,3,5",
      "[1 5,3,5]",
      "[]",
      "[,]",
      "[4, 5,]"
    )

  property("nonEmptyIntList should parse valid lists correctly") {
    forAll(validLists) { (input: String, expected: List[Int]) =>
      val Some((actual, rest)) = input.parse(nonEmptyIntList)
      actual shouldEqual expected
      rest shouldBe ""
    }
  }

  property("nonEmptyIntList should fail with invalid lists") {
    forAll(invalidLists) { input =>
      input.parse(nonEmptyIntList) shouldEqual None
    }
  }
}
