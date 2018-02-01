package parsers

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import parsers.ArithmeticExpressions._

class ArithmeticExpressionsTests
    extends PropSpec
    with PropertyChecks
    with Matchers {

  val validExpressions =
    Table(
      ("expression", "result"),
      ("42", 42),
      ("(((((42)))))", 42),
      ("1+1", 2),
      ("(1+1)", 2),
      ("1*1", 1),
      ("1*2", 2),
      ("(1*2)", 2),
      ("2*3+4", 10),
      ("2*(3+4)", 14),
      ("2 * 3 +  4", 10),
      ("2*(     3+ 4)  ", 14),
      ("2*3", 6),
      ("((1))*(2+(((3)))*(4+(((5))+6))*(((7*8)))+9)", 2531)
    )

  val invalidExpressions =
    Table(
      "expressions",
      "-1",
      "()",
      "(5",
      "(1+2",
      "(1+2()"
    )

  property("eval should evaluate valid expressions correctly") {
    forAll(validExpressions) { (expr: String, res: Int) =>
      val Right(actual) = eval(expr)
      actual shouldEqual res
    }
  }

  property("eval should fail with invalid expressions") {
    forAll(invalidExpressions) { expr =>
      val Left(err) = eval(expr)
      err shouldEqual "invalid input"
    }
  }
}
