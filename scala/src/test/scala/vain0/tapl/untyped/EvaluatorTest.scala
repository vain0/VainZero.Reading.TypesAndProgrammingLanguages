package vain0.tapl.untyped
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks

class EvaluatorTest
  extends FunSuite
    with TableDrivenPropertyChecks
    with ExpressionHelper {
  val id: Expression[Int] = vFun("x")(v0)
  test("evaluateOne") {
    val advanceCases =
      Table(
        ("expression", "expected"),
        (vApp(id)(id), id),
        (
          vApp(vApp(id)(id))(id),
          vApp(id)(id)
        ),
        (
          vApp(id)(vApp(id)(id)),
          vApp(id)(id)
        )
      )
    forAll(advanceCases) { (expression, expected) =>
      assertResult(expected) { Evaluator.evaluateOne(expression) }
    }

    val stuckCases =
      Table(
        "expression",
        v0,
        id,
        vFun("x")(vApp(id)(id))
      )
    forAll(stuckCases) { expression =>
      assertResult(expression) { Evaluator.evaluateOne(expression) }
    }
  }

  test("evaluateMany") {
    val expression = vApp(vFun("x")(vApp(id)(id)))(id)
    assert(Evaluator.evaluateMany(expression) == id)
  }
}
