package vain0.tapl.untyped

abstract class Evaluator {
  def evaluate(expression: Expression[Int]): Expression[Int]
}
