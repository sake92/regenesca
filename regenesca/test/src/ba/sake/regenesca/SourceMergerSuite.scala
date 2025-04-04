package ba.sake.regenesca

import scala.meta._
import scala.meta.contrib._
import scala.meta.dialects.Scala34

class SourceMergerSuite extends BaseSuite {

  val sourceMerger = SourceMerger()

  test("should just write generated Source if first is empty") {
    val first = source""
    val generated = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  override def routes = Routes {
    case GET() -> Path("vets") =>
      val pageReq = Request.current.queryParamsValidated[PageRequest]
      Response.withStatus(200)
  }
}
    """
    val result = sourceMerger.merge(first, generated).parse[Source].get
    assert(result.isEqual(generated))
  }

  test("should not touch anything if generated Source is the same") {
    val first = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  override def routes = Routes {
    case GET() -> Path("vets") =>
      val pageReq = Request.current.queryParamsValidated[PageRequest]
      Response.withStatus(200)
  }
}
    """
    val generated = first
    val result1 = sourceMerger.merge(first, generated).parse[Source].get
    assertEqStructure(result1, generated)
    // even the second time! idempotent
    val result2 = sourceMerger.merge(result1, generated).parse[Source].get
    assertEqStructure(result2, generated)
  }

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
    val result = sourceMerger.merge(first, generated).parse[Source].get
    assertEqStructure(result, expected)
  }

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

  test("should not reorder definitions") {
    val original =
      source"""
      class PetController {
        def routes = Routes {
          case PUT() -> Path("pet") =>
            val reqBody = Request.current.bodyJsonValidated[Pet]
            Response.withStatus(StatusCodes.NOT_IMPLEMENTED).withBody("TODO: return Pet")
          case POST() -> Path("pet") =>
            val reqBody = Request.current.bodyJsonValidated[Pet]
            Response.withStatus(StatusCodes.NOT_IMPLEMENTED).withBody("TODO: return Pet")
          case GET() -> Path("pet", "findByStatus") =>
            enum QpStatus derives QueryStringRW { case available, pending, sold }
            case class QP(status: Option[QpStatus]) derives QueryStringRW
            val qp = Request.current.queryParamsValidated[QP]
            Response.withStatus(StatusCodes.NOT_IMPLEMENTED).withBody("TODO: return Seq[Pet]")
        }
      }
      """
    val result1 = sourceMerger.merge(original, original).parse[Source].get
    assertEqStructure(result1, original)
    // and again..
    val result2 = sourceMerger.merge(result1, original).parse[Source].get
    assertEqStructure(result2, original)
  }

  test("should not reorder definitions v2") {
    val generated =
      source"""
package ba.sake.petstore.controllers
import io.undertow.util.StatusCodes
import ba.sake.querson.QueryStringRW
import ba.sake.sharaf.*, routing.*
import ba.sake.petstore.models.*
class UserController {
  def routes = Routes {
    case POST() -> Path("user") =>
      val reqBody = Request.current.bodyJsonValidated[User]
      Response.withStatus(StatusCodes.NOT_IMPLEMENTED).withBody("TODO: return User")
    case POST() -> Path("user", "createWithList") =>
      val reqBody = Request.current.bodyJsonValidated[Seq[User]]
      Response.withStatus(StatusCodes.NOT_IMPLEMENTED).withBody("TODO: return User")
    case GET() -> Path("user", "login") =>
      case class QP(username: Option[String], password: Option[String]) derives QueryStringRW
      val qp = Request.current.queryParamsValidated[QP]
      Response.withStatus(StatusCodes.NOT_IMPLEMENTED).withBody("TODO: return String")
    case GET() -> Path("user", "logout") =>
      Response.withStatus(StatusCodes.NOT_IMPLEMENTED)
    case GET() -> Path("user", username) =>
      Response.withStatus(StatusCodes.NOT_IMPLEMENTED).withBody("TODO: return User")
    case PUT() -> Path("user", username) =>
      val reqBody = Request.current.bodyJsonValidated[User]
      Response.withStatus(StatusCodes.NOT_IMPLEMENTED)
    case DELETE() -> Path("user", username) =>
      Response.withStatus(StatusCodes.NOT_IMPLEMENTED)
  }
}
      """
    val result1 = sourceMerger.merge(generated, generated).parse[Source].get
    assertEqStructure(result1, generated)
    // and again..
    val result2 = sourceMerger.merge(result1, generated).parse[Source].get
    assertEqStructure(result2, generated)
  }
}
