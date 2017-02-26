package vain0.tapl.untyped
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks

class ExpressionTest
  extends FunSuite
    with TableDrivenPropertyChecks
    with ExpressionHelper {
  test("varNameToIndex") {
    val successCases =
      Table(
        ("source", "expected"),
        (vFun("x")(x), vFun("x")(v0)),
        (
          vFun("x")(vApp(x)(vFun("y")(vApp(x)(y)))),
          vFun("x")(vApp(v0)(vFun("y")(vApp(v1)(v0))))
        )
      )
    forAll(successCases) { (source, expected) =>
      assertResult(Right(expected)) { source.varNameToIndex }
    }
    val failureCases =
      Table(
        "expression",
        vFun("x")(y),
        vFun("y")(vApp(x)(vFun("x")(y)))
      )
    forAll(failureCases) { expression =>
      assertResult(true) { expression.varNameToIndex.isLeft }
    }
  }
}
