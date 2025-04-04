package ba.sake.regenesca

import scala.meta._
import scala.meta.dialects.Scala34

class SourceMergerEnumsSuite extends BaseSuite {

  val sourceMerger = SourceMerger()

  test("should blindly overwrite enums") {
    val first = source"""
      enum Color:
        case Red
    """
    val generated = source"""
      enum Color:
        case Red, Blue
    """
    val expected = source"""
      enum Color:
        case Red, Blue
    """
    val result = sourceMerger.merge(first, generated).parse[Source].get
    assertEqStructure(result, expected)
  }

}
