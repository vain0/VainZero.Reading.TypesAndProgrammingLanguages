package vain0.tapl.untyped
import scala.util.parsing.combinator._

object Parsers extends RegexParsers {
  private def identifierParser =
    """[a-zA-Z_][a-zA-Z0-9_'-]*""".r

  private def varExpressionParser =
    identifierParser ^^ (name => VarExpression(name))

  private def parenExpressionParser =
    "(" ~> expressionParser <~ ")"

  private def atomicExpressionParser = (
    varExpressionParser
    | parenExpressionParser
  )

  private def appExpressionParser = {
    val separatorParser =
      success((f: Expression[String], x: Expression[String]) =>
          AppExpression(f, x)
      )
    chainl1(atomicExpressionParser, separatorParser)
  }

  private def funExpressionParser = (
    ("""\""" ~> identifierParser) ~ ("." ~> expressionParser)
    ^^ { case name ~ body => FunExpression(name, body) }
  )

  def expressionParser: Parser[Expression[String]] =
    funExpressionParser | appExpressionParser

  def parseExpression(source: String): Either[String, Expression[String]] =
    parseAll(expressionParser, source) match {
      case Success(expression, _) =>
        Right(expression)
      case NoSuccess(message, _) =>
        Left(message)
    }
}
