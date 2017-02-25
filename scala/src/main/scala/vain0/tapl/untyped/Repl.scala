package vain0.tapl.untyped

import io._

object Repl {
  private val evaluator = SmallStepEvaluator
  private val linebreak = "\r\n"

  private def evaluatePrint(source: String) =
    evaluator.evaluateSource(source) match {
      case Left(message) =>
        Console.err.println(message)
      case Right(expression) =>
        Console.out.println(expression.varIndexToName.prettyPrint)
    }

  private def tryEvaluatePrint(source: String) =
    evaluator.evaluateSource(source) match {
      case Left(_) =>
        false
      case Right(expression) =>
        Console.out.println(expression.varIndexToName.prettyPrint)
        true
    }

  def repl(source: Option[String]): Unit = {
    print(source match {
      case Some(_) => "| "
      case None => "> "
    })

    StdIn.readLine() match {
      case null | "quit" | "exit" =>
        ()
      case "" =>
        source match {
          case Some(source) =>
            evaluatePrint(source)
          case None =>
        }
        repl(None)
      case line =>
        val nextSource =
          source match {
            case Some(previousSource) =>
              val source = previousSource + linebreak + line
              if (tryEvaluatePrint(source)) None else Some(source)
            case None =>
              if (tryEvaluatePrint(line)) None else Some(line)
          }
        repl(nextSource)
    }
  }

  def main(args: Array[String]): Unit = {
    repl(None)
  }
}
