package vain0.tapl.untyped

case class NameContext(bindings: List[(String, Binding)]) {
  val length: Int = bindings.length

  def addBinding(name: String)(binding: Binding): NameContext =
    NameContext((name, binding) :: bindings)

  def addName(name: String): NameContext =
    addBinding(name)(NameBinding)

  def isNameBound(name: String): Boolean =
    bindings.exists { case (boundName, _) => boundName == name }

  def freshName(name: String): String =
    if (isNameBound(name)) {
      freshName(name + "'")
    } else {
      name
    }

  def addFreshName(name: String): NameContext =
    addName(freshName(name))

  def indexToName(index: Int): String =
    bindings(index)._1

  // TODO: This can be tail-recursive.
  def nameToIndex(name: String): Either[String, Int] =
    bindings match {
      case Nil =>
        Left(s"Undefined variable: $name")
      case (boundName, _) :: restBindings =>
        if (boundName == name) {
          Right(0)
        } else {
          val context = NameContext(restBindings)
          context.nameToIndex(name).map { index => index + 1 }
        }
    }
}

object NameContext {
  def empty = NameContext(List.empty)
}
