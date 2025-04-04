package ba.sake.regenesca

import scala.meta._
import scala.meta.dialects.Scala34

class SourceMergerClassesSuite extends BaseSuite {

  val sourceMerger = SourceMerger()

  test("should add new vals and defs, leaving existing ones intact") {
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
    val generated = source"""
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
    val result = sourceMerger.merge(first, generated).parse[Source].get
    assertEqStructure(result, expected)
  }

  test(
    "should overwrite methods completely when mergeDefBody = false"
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
    val result = SourceMerger(mergeDefBodies = false).merge(first, generated).parse[Source].get
    assertEqStructure(result, expected)
  }

  test("should add a class parameter") {
    locally {
      val original = source"""  class Pet() """
      val generated = source"""  class Pet(name: String) """
      val expected = source"""  class Pet(name: String) """
      val merged = sourceMerger.merge(original, generated).parse[Source].get
      assertEqStructure(merged, expected)
    }
    locally {
      val original = source"""  class Pet(id: Option[Long]) """
      val generated = source"""  class Pet(id: Option[Long], name: String) """
      val expected = source"""  class Pet(id: Option[Long], name: String) """
      val merged = sourceMerger.merge(original, generated).parse[Source].get
      assertEqStructure(merged, expected)
    }
  }

  test("should add a class parameter list") {
    val original = source""" case class Pet(id: Option[Long]) """
    val generated = source""" case class Pet(id: Option[Long], name: String)(email: String)  """
    val merged = sourceMerger.merge(original, generated).parse[Source].get
    assertEqStructure(merged, generated)
  }

  test("should add a modifier") {
    val original = source""" final class Pet (id: Option[Long]) """
    val generated = source""" final class  Pet private(id: Option[Long]) """
    val merged = sourceMerger.merge(original, generated).parse[Source].get
    assertEqStructure(merged, generated)
  }

}
