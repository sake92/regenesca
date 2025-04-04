package ba.sake.regenesca

import scala.meta.Source
//import scala.meta.contrib._

trait SourceMergerTest extends munit.FunSuite {
  
  def assertEqStructure(obtained: Source, expected: Source, debug: Boolean = true): Unit = {
    if (debug) {
      println("*" * 50)
      println(obtained.syntax)
    }
    assertEquals(obtained.structure, expected.structure, obtained.syntax)
  }
}
