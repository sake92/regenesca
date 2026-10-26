package ba.sake.regenesca

import scala.meta._
import scala.meta.dialects.Scala34

class ForComprehensionPrinterSuite extends BaseSuite {
  test("prints a for-yield inside a def") {
    val tree = "def route = for (a <- as; b <- bs(a)) yield b".parse[Source].get
    val printed = ForComprehensionPrinter.syntax(tree)

    assertEquals(
      printed,
      """def route = for {
        |              a <- as
        |              b <- bs(a)
        |            } yield b""".stripMargin
    )
    assertEquals(printed.parse[Source].get.structure, tree.structure)
  }

  test("prints nested for-yields inside a def") {
    val tree = "def nested = for (a <- outer) yield for (b <- inner(a); if b.ready) yield b".parse[Source].get
    val printed = ForComprehensionPrinter.syntax(tree)

    assertEquals(printed.parse[Source].get.structure, tree.structure)
    assert(!printed.contains(";"))
    assertEquals(printed.count(_ == '{'), 2)
  }

  test("delegates source without comprehensions to Scalameta") {
    val tree = "class Api { def answer = 42 }".parse[Source].get

    assertEquals(ForComprehensionPrinter.syntax(tree), tree.syntax)
  }

  test("preserves comments inside qualifiers") {
    val source = """def route = for {
                   |  a <- as // keep this comment
                   |  b <- bs(a)
                   |} yield b""".stripMargin
    val tree = source.parse[Source].get

    assertEquals(ForComprehensionPrinter.syntax(tree), source)
  }
}
