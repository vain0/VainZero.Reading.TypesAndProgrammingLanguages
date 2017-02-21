package vain0.tapl.untyped

object Evaluator {
  // Evaluates one step.
  def EvaluateOne(expression: Expression[Int]): Expression[Int] = {
    expression match {
      case AppExpression(FunExpression(_, body), argument)
        if argument.isValue =>
        body.substituteTop(argument)
      case AppExpression(function, argument)
        if function.isValue =>
        val argument1 = EvaluateOne(argument)
        AppExpression(function, argument1)
      case AppExpression(function, argument) =>
        val function1 = EvaluateOne(function)
        AppExpression(function1, argument)
      case
        VarExpression(_)
        | FunExpression(_, _) =>
        expression
    }
  }

  // Evaluates step by step as possible.
  def EvaluateMany(expression: Expression[Int]): Expression[Int] = {
    val expression1 = EvaluateOne(expression)
    if (expression == expression1) {
      expression
    } else {
      EvaluateMany(expression1)
    }
  }
}
