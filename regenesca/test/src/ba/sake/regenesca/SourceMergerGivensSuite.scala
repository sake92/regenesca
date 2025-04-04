package ba.sake.regenesca

import scala.meta._
import scala.meta.dialects.Scala34

class SourceMergerGivensSuite extends BaseSuite {

  val sourceMerger = SourceMerger()

  test("should not reorder given aliases") {
    val original = source"""
      object Model {
        given Configuration = Configuration.default
        given Codec[Model] = ConfiguredCodec.derived
      }
    """
    val result1 = sourceMerger.merge(original, original)
    assertEqStructure(result1.parse[Source].get, original)
    // and again..
    val result2 = sourceMerger.merge(result1.parse[Source].get, original)
    assertEqStructure(result2.parse[Source].get, original)
  }

  test("should not reorder given definitions") {
    val original = source"""
      object Model {
        given Configuration with {
          def bla = ""
        }
        given Codec[Model] with { }
      }
    """
    val result1 = sourceMerger.merge(original, original)
    assertEqStructure(result1.parse[Source].get, original)
    // and again..
    val result2 = sourceMerger.merge(result1.parse[Source].get, original)
    assertEqStructure(result2.parse[Source].get, original)
  }
}
