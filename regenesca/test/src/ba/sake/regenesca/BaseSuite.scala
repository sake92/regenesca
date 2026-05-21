package ba.sake.regenesca

import scala.meta._
//import scala.meta.contrib._

trait BaseSuite extends munit.FunSuite {

  def assertEqStructure(obtained: String, expected: Source, debug: Boolean = false)(implicit d: Dialect): Unit = {
    if (debug) {
      println("*" * 50)
      println(obtained)
    }
    val obtainedSource = ParseUtils.parseSourceOrThrow(Input.String(obtained), "assertEqStructure obtained")
    assertEquals(obtainedSource.structure, expected.structure, obtained)
  }
}
