package vain0.tapl.untyped
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks

class ParsersTest
  extends FunSuite
    with TableDrivenPropertyChecks
    with ExpressionHelper {
  test("expressionParser") {
    val successCases =
      Table(
        ("source", "expected"),
        ("x", x),
        ("x y", vApp(x)(y)),
        ("x y z", vApp(vApp(x)(y))(z)),
        ("\\x. x", vFun("x")(x)),
        (
          "\\x. \\y. x",
          vFun("x")(vFun("y")(x))
        ),
        (
          "\\x. \\y. x y",
          vFun("x")(vFun("y")(vApp(x)(y)))
        ),
        ("(\\x. x) x", vApp(vFun("x")(x))(x))
      )

    forAll(successCases) { (source, expected) =>
      Parsers.parseAll(Parsers.expressionParser, source) match {
        case Parsers.Success(result, _) =>
          assertResult(expected) {
            result
          }
        case Parsers.NoSuccess(message, _) =>
          fail(message)
      }
    }

    val failureCases =
      Table(
        "source",
        "x!",
        "()"
      )

    forAll(failureCases) { source =>
      Parsers.parseAll(Parsers.expressionParser, source) match {
        case Parsers.Success(result, _) =>
          fail(s"Unexpected parse success: $result")
        case Parsers.NoSuccess(_, _) =>
          ()
      }
    }
  }
}
