package vain0.tapl.untyped

trait ExpressionHelper {
  def vVar[V](v: V) =
    VarExpression(v)

  def vApp[V](function: Expression[V])(argument: Expression[V]) =
    AppExpression(function, argument)

  def vFun[V](parameter: String)(body: Expression[V]) =
    FunExpression(parameter, body)

  val x: Expression[String] = vVar("x")
  val y: Expression[String] = vVar("y")
  val z: Expression[String] = vVar("z")
  val w: Expression[String] = vVar("w")

  val v0: Expression[Int] = vVar(0)
  val v1: Expression[Int] = vVar(1)
  val v2: Expression[Int] = vVar(2)
}
