package vain0.tapl.untyped
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks

class EvaluatorTest
  extends FunSuite
    with TableDrivenPropertyChecks
    with ExpressionHelper {
  val id: Expression[Int] = vFun("x")(v0)
  test("EvaluateOne") {
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
      assertResult(expected) { Evaluator.EvaluateOne(expression) }
    }

    val stuckCases =
      Table(
        "expression",
        v0,
        id,
        vFun("x")(vApp(id)(id))
      )
    forAll(stuckCases) { expression =>
      assertResult(expression) { Evaluator.EvaluateOne(expression) }
    }
  }

  test("EvaluateMany") {
    val expression = vApp(vFun("x")(vApp(id)(id)))(id)
    assert(Evaluator.EvaluateMany(expression) == id)
  }
}
