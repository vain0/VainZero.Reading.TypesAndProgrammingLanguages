package vain0.tapl.untyped

class SmallStepEvaluatorTest extends EvaluatorTest {
  override val evaluator = SmallStepEvaluator

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
}
