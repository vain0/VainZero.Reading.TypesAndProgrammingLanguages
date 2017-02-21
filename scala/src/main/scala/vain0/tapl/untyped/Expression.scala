package vain0.tapl.untyped

sealed abstract class Expression[V] {
  def isValue: Boolean =
    this match {
      case VarExpression(_)
           | AppExpression(_, _) => false
      case FunExpression(_, _) => true
    }

  // Maps variables, calculating the depth.
  def mapVar[U](c: Int)(onVar: (V, Int) => Expression[U]): Expression[U] = {
    this match {
      case VarExpression(v) =>
        onVar(v, c)
      case FunExpression(parameter, body) =>
        FunExpression(parameter, body.mapVar(c + 1)(onVar))
      case AppExpression(f, x) =>
        AppExpression(f.mapVar(c)(onVar), x.mapVar(c)(onVar))
    }
  }
}

case class VarExpression[V](variable: V)
  extends Expression[V]

case class AppExpression[V](function: Expression[V], argument: Expression[V])
  extends Expression[V]

case class FunExpression[V](parameter: String, body: Expression[V])
  extends Expression[V]

object Expression {
  implicit class RichStringExpression(val expression: Expression[String]) {
    // Replace variables with their de Bruijn indexes.
    def varNameToIndex: Either[String, Expression[Int]] = {
      def loop(expression: Expression[String], nameContext: NameContext): Either[String, Expression[Int]] = {
        expression match {
          case VarExpression(name) =>
            nameContext.nameToIndex(name).map { index =>
              VarExpression(index)
            }
          case FunExpression(parameter, body) =>
            loop(body, nameContext.addName(parameter)).map { body =>
              FunExpression(parameter, body)
            }
          case AppExpression(function, argument) =>
            for {
              function <- loop(function, nameContext)
              argument <- loop(argument, nameContext)
            } yield {
              AppExpression(function, argument)
            }
        }
      }
      loop(expression, NameContext.empty)
    }

    def prettyPrint: String = {
      expression match {
        case VarExpression(name) => name
        case FunExpression(parameter, body) =>
          s"(\\$parameter. ${body.prettyPrint})"
        case AppExpression(function, argument) =>
          s"(${function.prettyPrint} ${argument.prettyPrint})"
      }
    }
  }

  implicit class RichIntExpression(val expression: Expression[Int]) {
    def varIndexToName: Expression[String] = {
      def loop(expression: Expression[Int], nameContext: NameContext): Expression[String] = {
        expression match {
          case VarExpression(index) =>
            val name = nameContext.indexToName(index)
            VarExpression(name)
          case FunExpression(parameter, body) =>
            val body1 = loop(body, nameContext.addName(parameter))
            FunExpression(parameter, body1)
          case AppExpression(function, argument) =>
            val function1 = loop(function, nameContext)
            val argument1 = loop(argument, nameContext)
            AppExpression(function1, argument1)
        }
      }
      loop(expression, NameContext.empty)
    }

    def shiftAbove(d: Int)(c: Int): Expression[Int] = {
      expression.mapVar(c) { (v, c) =>
        val u =
          if (v >= c) {
            v + d
          } else {
            v
          }
        VarExpression(u)
      }
    }

    def shift(d: Int): Expression[Int] = {
      shiftAbove(d)(0)
    }

    def substitute(j: Int)(substance: Expression[Int]): Expression[Int] = {
      expression.mapVar(0) { (v, c) =>
        if (v == j + c) {
          substance.shift(c)
        } else {
          VarExpression(v)
        }
      }
    }

    def substituteTop(substance: Expression[Int]): Expression[Int] = {
      substitute(0)(substance.shift(1)).shift(-1)
    }
  }
}
