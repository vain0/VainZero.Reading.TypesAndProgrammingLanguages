package vain0.tapl.untyped

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks

abstract class EvaluatorTest
  extends FunSuite
    with TableDrivenPropertyChecks
    with ExpressionHelper {
  val evaluator: Evaluator
  val id: Expression[Int] = vFun("x")(v0)

  test("evaluate") {
    val expression = vApp(vFun("x")(vApp(id)(id)))(id)
    assert(evaluator.evaluate(expression) == id)
  }
}
