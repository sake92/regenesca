package ba.sake.regenesca

import scala.meta._
import scala.meta.contrib._
import scala.meta.dialects.Scala34

class SourceMergerSuite extends munit.FunSuite {

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
    val result = SourceMerger().merge(first, second)
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
    val result = SourceMerger().merge(first, second)
    assertEquals(result.structure, second.structure)
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
    val excepted = source"""
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
    val result = SourceMerger().merge(first, second)
    assertEquals(result.structure, excepted.structure)
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
    val excepted = source"""
package ba.sake.sharaf.petclinic.web.controllers
import ba.sake.sharaf.*, routing.*
class VetController() extends SharafController {
  val oldVal1 = "ccc"
  val oldVal2 = "bbb"
}
    """
    val result = SourceMerger().merge(first, second)
    assertEquals(result.structure, excepted.structure)
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
    val excepted = source"""
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
    val result = SourceMerger().merge(first, second)
    assertEquals(result.structure, excepted.structure)
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
    val excepted = source"""
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
    val result = SourceMerger(mergeDefBody = false).merge(first, second)
    assertEquals(result.structure, excepted.structure)
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
    val excepted = source"""
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
    val result = SourceMerger().merge(first, second)
    assertEquals(result.structure, excepted.structure)
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
    val excepted = source"""
enum Color:
  case Red, Blue
    """
    val result = SourceMerger().merge(first, second)
    assertEquals(result.structure, excepted.structure)
  }
}
