package vain0.tapl.untyped

import io._

object Repl {
  private def evaluate(source: String) =
    for {
      nominalExpression <- Parsers.parseExpression(source)
      indexedExpression <- nominalExpression.varNameToIndex
      value = Evaluator.EvaluateMany(indexedExpression)
    } yield value

  private def evaluatePrint(source: String) =
    evaluate(source) match {
      case Left(message) =>
        Console.err.println(message)
      case Right(expression) =>
        Console.out.println(expression.varIndexToName.prettyPrint)
    }

  def repl(): Unit = {
    StdIn.readLine() match {
      case null | "quit" | "exit" =>
        ()
      case "" =>
        repl()
      case line =>
        evaluatePrint(line)
        repl()
    }
  }

  def main(args: Array[String]): Unit = {
    repl()
  }
}
