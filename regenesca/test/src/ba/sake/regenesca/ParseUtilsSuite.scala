package ba.sake.regenesca

import scala.meta.Input

class ParseUtilsSuite extends munit.FunSuite {

  test("parseSourceOrThrow includes context and location") {
    val ex = intercept[IllegalArgumentException] {
      ParseUtils.parseSourceOrThrow(Input.String("def x = "), "unit-test-context")
    }
    assert(clue(ex.getMessage).contains("unit-test-context"))
    assert(clue(ex.getMessage).contains("Failed to parse source"))
    assert(clue(ex.getMessage).contains(":"))
  }
}
