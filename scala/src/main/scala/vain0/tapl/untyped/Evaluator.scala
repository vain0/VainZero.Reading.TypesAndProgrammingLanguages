package vain0.tapl.untyped

abstract class Evaluator {
  def evaluate(expression: Expression[Int]): Expression[Int]

  def evaluateSource(source: String): Either[String, Expression[Int]] =
    for {
      nominalExpression <- Parsers.parseExpression(source)
      indexedExpression <- nominalExpression.varNameToIndex
      value = SmallStepEvaluator.evaluate(indexedExpression)
    } yield value
}
