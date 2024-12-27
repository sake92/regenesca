package ba.sake.regenesca

import scala.meta._
import scala.meta.contrib._
import scala.meta.dialects.Scala34

class SourceMergerSuite extends munit.FunSuite {

  val sourceMerger = SourceMerger()

  test("SourceMerger.merge should just write second Source if first is empty") {
    val first = source""
    val second = source"""
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
    val result = sourceMerger.merge(first, second)
    assert(result.isEqual(second))
  }

  test(
    "SourceMerger.merge should not touch anything if second Source is the same"
  ) {
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
    val second = first
    val result1 = sourceMerger.merge(first, second)
    assertEquals(result1.structure, second.structure)
    // even the second time! idempotent
    val result2 = sourceMerger.merge(result1, second)
    assertEquals(result2.structure, second.structure)
  }

  test(
    "SourceMerger.merge should add new vals and defs, leaving existing ones intact"
  ) {
    val first = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  val oldVal = "abc"
  override def routes = Routes {
    case GET() -> Path("vets") =>
      val pageReq = Request.current.queryParamsValidated[PageRequest]
      Response.withStatus(200)
  }
  def oldMethod: Int = ???
}
    """
    val second = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  val newVal = "def"
  override def routes = Routes {
    case GET() -> Path("vets") =>
      val pageReq = Request.current.queryParamsValidated[PageRequest]
      Response.withStatus(200)
  }
  def newMethod: Int = ???
}
    """
    val expected = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  val oldVal = "abc"
  val newVal = "def"
  override def routes = Routes {
    case GET() -> Path("vets") =>
      val pageReq = Request.current.queryParamsValidated[PageRequest]
      Response.withStatus(200)
  }
  def oldMethod: Int = ???
  def newMethod: Int = ???
}
    """
    val result = sourceMerger.merge(first, second)
    assertEquals(result.structure, expected.structure)
  }

  test(
    "SourceMerger.merge should overwrite same-named vals, leaving existing ones intact"
  ) {
    val first = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  val oldVal1 = "aaa"
  val oldVal2 = "bbb"
}
    """
    val second = source"""
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
    val result = sourceMerger.merge(first, second)
    assertEquals(result.structure, expected.structure)
  }

  // this is not to screw up existing code
  test(
    "SourceMerger.merge should overwrite vals inside methods, but leave expressions intact"
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
    val second = source"""
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
    val result = sourceMerger.merge(first, second)
    assertEquals(result.structure, expected.structure)
  }

  test(
    "SourceMerger.merge should overwrite methods completely when mergeDefBody = false"
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
    val second = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  def oldDef1 = {
    val x = 42
    x + y
  }
}
    """
    val expected = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  def oldDef1 = {
    val x = 42
    x + y
  }
  def oldDef2 = "bbb"
}
    """
    val result = SourceMerger(mergeDefBodies = false).merge(first, second)
    assertEquals(result.structure, expected.structure)
  }

  test("SourceMerger.merge should add new cases to partial function") {
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
    val second = source"""
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
    val result = sourceMerger.merge(first, second)
    assertEquals(result.structure, expected.structure)
  }

  test("SourceMerger.merge should add new vals in case body") {
    val first = source"""
      def routes = Routes {
        case GET() -> Path("vets") =>
          Response.withStatus(200).withBody("whatever")
        case GET() -> Path("vets2") =>
          Response.withStatus(200).withBody("whatever", "2 args")
      }
    """
    val second = source"""
      def routes = Routes {
        case GET() -> Path("vets") =>
          val pageReq = Request.current.queryParamsValidated[NewQP]
          Response.withStatus(200).withBody("whatever")
        case GET() -> Path("vets2") =>
          val pageReq = Request.current.queryParamsValidated[NewQP]
          Response.withStatus(200).withBody("whatever", "2 args")
      }
    """
    val result = sourceMerger.merge(first, second)
    println(result.syntax)
    assertEquals(result.structure, second.structure)
  }

  test(
    "SourceMerger.merge should blindly overwrite enums"
  ) {
    val first = source"""
    enum Color:
      case Red
    """
    val second = source"""
    enum Color:
      case Red, Blue
    """
    val expected = source"""
    enum Color:
      case Red, Blue
    """
    val result = sourceMerger.merge(first, second)
    assertEquals(result.structure, expected.structure)
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
    val result1 = sourceMerger.merge(original, original)
    assertEquals(result1.structure, original.structure)
    // and again..
    val result2 = sourceMerger.merge(result1, original)
    assertEquals(result2.structure, original.structure)
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
    val result1 = sourceMerger.merge(generated, generated)
    assertEquals(result1.structure, generated.structure)
    // and again..
    val result2 = sourceMerger.merge(result1, generated)
    assertEquals(result2.structure, generated.structure)
  }

  test("should add a class parameter") {
    val original = source"""  class Pet(id: Option[Long]) """
    val generated = source"""  class Pet(name: String) """
    val expected = source"""  class Pet(id: Option[Long], name: String) """
    val result1 = sourceMerger.merge(original, generated)
    assertEquals(result1.structure, expected.structure)
  }

  test("should add a class parameter list") {
    val original = source""" case class Pet(id: Option[Long]) """
    val generated = source""" case class Pet(id: Option[Long], name: String)(email: String)  """
    val result1 = sourceMerger.merge(original, generated)
    assertEquals(result1.structure, generated.structure)
  }

  test("should add a modifier") {
    val original = source""" final class Pet(id: Option[Long]) """
    val generated = source""" final class  Pet private(id: Option[Long]) """
    val result1 = sourceMerger.merge(original, generated)
    assertEquals(result1.structure, generated.structure)
  }

}
