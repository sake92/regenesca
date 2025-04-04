package ba.sake.regenesca

import scala.meta._
import scala.meta.dialects.Scala34

class SourceMergerValsVarsSuite extends SourceMergerTest {

  val sourceMerger = SourceMerger()

  test("should overwrite same-named vals, leaving existing ones intact") {
    val first = source"""
      val oldVal1 = "aaa"
      val oldVal2 = "bbb"
    """
    val generated = source"""
      val oldVal1 = "ccc"
    """
    val expected = source"""
      val oldVal1 = "ccc"
      val oldVal2 = "bbb"
    """
    val result = sourceMerger.merge(first, generated)
    assertEqStructure(result.parse[Source].get, expected)
  }

  // this is not to screw up existing code
  test("should overwrite vals inside methods, but leave expressions intact") {
    val first = source"""
      def oldDef1 = {
        val x = 1
        x + x
      }
      def oldDef2 = "bbb"
    """
    val generated = source"""
      def oldDef1 = {
        val x = 42
        x + y // SHOULD NOT BE TOUCHED !!!
      }
    """
    val expected = source"""
      def oldDef1 = {
        val x = 42
        x + x
      }
      def oldDef2 = "bbb"
    """
    val result = sourceMerger.merge(first, generated)
    assertEqStructure(result.parse[Source].get, expected)
  }

  test("should add new vals in case body") {
    val first = source"""
      def routes = Routes {
        case GET() -> Path("vets") =>
          val reqBody = Request.current.bodyJsonValidated[MyReq]
          Response.withStatus(200).withBody("whatever")
      }
    """
    val generated = source"""
      def routes = Routes {
        case GET() -> Path("vets") =>
          val reqBody = Request.current.bodyJsonValidated[MyReq]
          val qp = Request.current.queryParamsValidated[NewQP]
          Response.withStatus(200).withBody("whatever")
      }
    """
    val result = sourceMerger.merge(first, generated)
    assertEqStructure(result.parse[Source].get, generated)
  }
}
