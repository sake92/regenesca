package ba.sake.regenesca

import scala.meta._
import scala.meta.dialects.Scala34

class SourceMergerCasesSuite extends BaseSuite {

  val sourceMerger = SourceMerger()

  test("should add new cases to partial function") {
    val first = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  override def routes = Routes {
      case GET() -> Path("vets") =>
        val pageReq = Request.current.queryParamsValidated[PageRequest]
        Response.withStatus(200).withBody("whatever")
  }
}
    """
    val generated = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  override def routes = Routes {
    case GET() -> Path("vets") =>
      val pageReq = Request.current.queryParamsValidated[NewQP]
      Response.withStatus(200)
    case GET() -> Path("newroute") =>
      val pageReq = Request.current.queryParamsValidated[PageRequest]
      Response.withStatus(200)
  }
}
    """
    val expected = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  override def routes = Routes {
    case GET() -> Path("vets") =>
      val pageReq = Request.current.queryParamsValidated[NewQP]
      Response.withStatus(200).withBody("whatever") // old expr should be preserved!
    case GET() -> Path("newroute") =>
      val pageReq = Request.current.queryParamsValidated[PageRequest]
      Response.withStatus(200)
  }
}
    """
    val result = sourceMerger.merge(first, generated)
    assertEqStructure(result, expected)
  }

}
