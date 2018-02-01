package parsers

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop._

class ParsersTests extends PropSpec with PropertyChecks with Matchers {

  private val nonEmptyString =
    Gen.nonEmptyListOf(arbitrary[Char]).map(_.mkString)

  property("succeed should always succeed with the result value v") {
    forAll(nonEmptyString, arbitrary[Int]) { (input, v) =>
      parsers.succeed(v)(input) shouldEqual Some(v, input)
    }
  }

  property("fail should always fail") {
    forAll(arbitrary[String]) { input =>
      parsers.fail(input) shouldEqual None
    }
  }

  property("item should succeed with first char for non empty inputs") {
    forAll(nonEmptyString) { input =>
      item(input) shouldEqual Some(input.charAt(0), input.substring(1))
    }
  }

  property("item should fail if input is empty") {
    item("") shouldEqual None
  }

  property("sequencing") {
    val p = for {
      v1 <- item
      _ <- item
      v2 <- item
    } yield s"$v1$v2"

    "abc".parse(p) shouldEqual Some("ac", "")
  }

  property("choice") {
    forAll(arbitrary[String]) { input =>
      val p = parsers.fail +++ parsers.succeed(42)
      input.parse(p) shouldEqual Some(42, input)
    }
  }
}
