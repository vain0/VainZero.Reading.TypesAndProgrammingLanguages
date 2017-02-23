package vain0.tapl.untyped
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks

class SmallStepEvaluatorTest
  extends FunSuite
    with TableDrivenPropertyChecks
    with ExpressionHelper {
  val evaluator = SmallStepEvaluator
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
      assertResult(expected) { evaluator.evaluateOne(expression) }
    }

    val stuckCases =
      Table(
        "expression",
        v0,
        id,
        vFun("x")(vApp(id)(id))
      )
    forAll(stuckCases) { expression =>
      assertResult(expression) { evaluator.evaluateOne(expression) }
    }
  }

  test("evaluate") {
    val expression = vApp(vFun("x")(vApp(id)(id)))(id)
    assert(evaluator.evaluate(expression) == id)
  }
}
