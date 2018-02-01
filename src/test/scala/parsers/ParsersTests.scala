package parsers

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop._

class ParsersTests extends PropSpec with PropertyChecks with Matchers {

  private val nonEmptyString =
    Gen.nonEmptyListOf(arbitrary[Char]).map(_.mkString)
  private val nonAlphaChar = arbitrary[Char].suchThat(c => !c.isLetter)
  private val nonAlphaNumChar =
    arbitrary[Char].suchThat(c => !c.isLetterOrDigit)

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

  property("digit should succeed on numeric char") {
    forAll(Gen.numChar, Gen.alphaNumStr) { (c, out) =>
      digit(s"$c$out") shouldEqual Some(c.toInt, out)
    }
  }

  property("digit should fail on alpha char") {
    forAll(Gen.alphaChar, Gen.alphaNumStr) { (c, out) =>
      digit(s"$c$out") shouldEqual None
    }
  }

  property("lower should succeed on lower case char") {
    forAll(Gen.alphaLowerChar, Gen.alphaNumStr) { (c, out) =>
      lower(s"$c$out") shouldEqual Some(c, out)
    }
  }

  property("lower should fail on upper case char") {
    forAll(Gen.alphaUpperChar, Gen.alphaNumStr) { (c, out) =>
      lower(s"$c$out") shouldEqual None
    }
  }

  property("lower should fail on non alpha char") {
    forAll(nonAlphaChar, Gen.alphaNumStr) { (c, out) =>
      lower(s"$c$out") shouldEqual None
    }
  }

  property("upper should succeed on upper case char") {
    forAll(Gen.alphaUpperChar, Gen.alphaNumStr) { (c, out) =>
      upper(s"$c$out") shouldEqual Some(c, out)
    }
  }

  property("upper should fail on lower case char") {
    forAll(Gen.alphaLowerChar, Gen.alphaNumStr) { (c, out) =>
      upper(s"$c$out") shouldEqual None
    }
  }

  property("upper should fail on non alpha char") {
    forAll(nonAlphaChar, Gen.alphaNumStr) { (c, out) =>
      upper(s"$c$out") shouldEqual None
    }
  }

  property("letter should succeed on alpha char") {
    forAll(Gen.alphaChar, Gen.alphaNumStr) { (c, out) =>
      letter(s"$c$out") shouldEqual Some(c, out)
    }
  }

  property("letter should fail on non alpha char") {
    forAll(nonAlphaChar, Gen.alphaNumStr) { (c, out) =>
      letter(s"$c$out") shouldEqual None
    }
  }

  property("alphaNum should succeed on alphaNum char") {
    forAll(Gen.alphaNumChar, Gen.alphaNumStr) { (c, out) =>
      alphaNum(s"$c$out") shouldEqual Some(c, out)
    }
  }

  property("alphaNum should fail on non alphaNum char") {
    forAll(nonAlphaNumChar, Gen.alphaNumStr) { (c, out) =>
      letter(s"$c$out") shouldEqual None
    }
  }

  property("char should succeed if input starts with specified char") {
    forAll(arbitrary[Char], Gen.alphaNumStr) { (c, out) =>
      char(c)(s"$c$out") shouldEqual Some(c, out)
    }
  }

  property("char should fail if input does not starts with specified char") {
    forAll(nonEmptyString) { input =>
      char((input.charAt(0).toInt + 1).toChar)(input) shouldEqual None
    }
  }

  property("string should succeed if input starts with specified string") {
    forAll(arbitrary[String], arbitrary[String]) { (str, out) =>
      val input = s"$str$out"
      input.parse(string(str)) shouldEqual Some(str, out)
    }
  }

  property("string should fail if input does not start with specified string") {
    forAll(nonEmptyString, arbitrary[String]) { (str, input) =>
      whenever(input.isEmpty || !input.startsWith(str)) {
        input.parse(string(str)) shouldEqual None
      }
    }
  }

  property("many should always succeed") {
    forAll(Gen.choose(0, 20), arbitrary[Char], arbitrary[String]) {
      (n, c, out) =>
        whenever(out.isEmpty || !out.startsWith(s"$c")) {
          val start = (1 to n).map(_ => c).mkString
          val input = s"$start$out"
          input.parse(many(char(c))) shouldEqual Some(
            (1 to n).map(_ => c).toList,
            out)
        }
    }
  }

  property("many1 should succeed with 'List(a, b, c)' for input 'abcFoo'") {
    "abcFoo".parse(many1(lower)) shouldEqual Some(List('a', 'b', 'c'), "Foo")
  }

  property("many1 should fail for empty inputs") {
    forAll(Gen.oneOf(item, lower, upper, letter, char('f'))) { p =>
      "".parse(many1(p)) shouldEqual None
    }
  }

  property("ident") {
    forAll(Gen.alphaLowerChar, Gen.alphaNumStr) { (x, xs) =>
      s"$x$xs".parse(ident) shouldEqual Some(s"$x$xs", "")
    }
  }

  property("ident fail") {
    forAll(Gen.alphaUpperChar, Gen.alphaNumStr) { (x, xs) =>
      s"$x$xs".parse(ident) shouldEqual None
    }
  }

  property("nat") {
    "123foo".parse(nat) shouldEqual Some(123, "foo")
  }

  property("nat fail") {
    forAll(Gen.alphaStr) { alphaStr =>
      alphaStr.parse(nat) shouldEqual None
    }
  }

  property("space") {
    "   foo".parse(space) shouldEqual Some((), "foo")
  }

  property("token") {
    forAll(arbitrary[String]) { str =>
      s"   $str   foo".parse(token(string(str))) shouldEqual Some(str, "foo")
      s"   ${str}foo".parse(token(string(str))) shouldEqual Some(str, "foo")
      s"$str foo".parse(token(string(str))) shouldEqual Some(str, "foo")
    }
  }

  property("token fail") {
    forAll(Gen.alphaUpperStr) { str =>
      str.parse(token(lower)) shouldEqual None
    }
  }

  property("identifier") {
    " someIdentifier foo".parse(identifier) shouldEqual Some("someIdentifier",
                                                             "foo")
  }

  property("natural") {
    "  123 foo".parse(natural) shouldEqual Some(123, "foo")
  }

  property("symbol") {
    "  foobar foo".parse(symbol("foobar")) shouldEqual Some("foobar", "foo")
  }
}
