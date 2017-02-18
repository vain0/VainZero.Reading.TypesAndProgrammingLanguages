package vain0.tapl.untyped

import io._

object Repl {
  def repl(): Unit = {
    StdIn.readLine() match {
      case null | "quit" | "exit" =>
        ()
      case "" =>
        repl()
      case line =>
        println(line + "!")
        repl()
    }
  }

  def main(args: Array[String]): Unit = {
    repl()
  }
}
