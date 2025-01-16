package ba.sake.regenesca

import scala.meta._
import scala.meta.contrib._
import scala.meta.dialects.Scala34

class SourceMergerValsVarsSuite extends munit.FunSuite {

  val sourceMerger = SourceMerger()

  test(
    "should overwrite same-named vals, leaving existing ones intact"
  ) {
    val first = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  val oldVal1 = "aaa"
  val oldVal2 = "bbb"
}
    """
    val generated = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  val oldVal1 = "ccc"
}
    """
    val expected = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  val oldVal1 = "ccc"
  val oldVal2 = "bbb"
}
    """
    val result = sourceMerger.merge(first, generated)
    assertEqStructure(result, expected)
  }

  // this is not to screw up existing code
  test(
    "should overwrite vals inside methods, but leave expressions intact"
  ) {
    val first = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  def oldDef1 = {
    val x = 1
    x + x
  }
  def oldDef2 = "bbb"
}
    """
    val generated = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  def oldDef1 = {
    val x = 42
    x + y // should NOT BE ACCEPTED !!!
  }
}
    """
    val expected = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  def oldDef1 = {
    val x = 42
    x + x
  }
  def oldDef2 = "bbb"
}
    """
    val result = sourceMerger.merge(first, generated)
    assertEqStructure(result, expected)
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
          enum QpStatus derives QueryStringRW { case eeeeee }
          case class QP(status: Option[QpStatus]) derives QueryStringRW
          val reqBody = Request.current.bodyJsonValidated[MyReq]
          val qp = Request.current.queryParamsValidated[NewQP]
          Response.withStatus(200).withBody("whatever")
      }
    """
    val result = sourceMerger.merge(first, generated)
    assertEqStructure(result, generated)
  }

  private def assertEqStructure(obtained: Source, expected: Source) =
    assertEquals(obtained.structure, expected.structure, obtained.syntax)
}
