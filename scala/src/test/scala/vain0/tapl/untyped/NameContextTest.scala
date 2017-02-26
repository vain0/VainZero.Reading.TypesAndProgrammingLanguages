package vain0.tapl.untyped
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks

class NameContextTest
  extends FunSuite
    with TableDrivenPropertyChecks
    with ExpressionHelper {
  private val xyzContext =
    NameContext.empty.addName("x").addName("y").addName("z")

  test("addName") {
    val expected =
      ("y", NameBinding) :: ("x", NameBinding) :: Nil
    assertResult(expected) {
      NameContext.empty.addName("x").addName("y").bindings
    }
  }

  test("isNameBound") {
    val c = xyzContext
    assert(c.isNameBound("x"))
    assert(c.isNameBound("z"))
    assert(!c.isNameBound("a"))
  }

  test("freshName") {
    val c = xyzContext.addName("x'")
    assertResult("x''")(c.freshName("x"))
    assertResult("y'")(c.freshName("y"))
    assertResult("a")(c.freshName("a"))
  }

  test("indexToName") {
    val c = xyzContext
    assertResult("z") { c.indexToName(0) }
    assertResult("x") { c.indexToName(2) }
  }

  test("nameToIndex") {
    val c = xyzContext
    assertResult(Right(2)) { c.nameToIndex("x") }
    assertResult(Right(0)) { c.nameToIndex("z") }
    assert(c.nameToIndex("a").isLeft)
  }
}
