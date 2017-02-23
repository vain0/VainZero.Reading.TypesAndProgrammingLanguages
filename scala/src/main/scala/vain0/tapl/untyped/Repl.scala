package vain0.tapl.untyped

import io._

object Repl {
  val evaluator = SmallStepEvaluator

  private def evaluatePrint(source: String) =
    evaluator.evaluateSource(source) match {
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
