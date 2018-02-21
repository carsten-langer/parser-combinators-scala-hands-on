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
      parsers.succeed(v)(input) shouldEqual Some((v, input))
    }
  }

  property("fail should always fail") {
    forAll(arbitrary[String]) { input =>
      parsers.fail(input) shouldEqual None
    }
  }

  property("anyChar should succeed with first char for non empty inputs") {
    forAll(nonEmptyString) { input =>
      anyChar(input) shouldEqual Some((input.charAt(0), input.substring(1)))
    }
  }

  property("anyChar should fail if input is empty") {
    anyChar("") shouldEqual None
  }
}
