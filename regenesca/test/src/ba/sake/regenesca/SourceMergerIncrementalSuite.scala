package ba.sake.regenesca

import scala.meta._
import scala.meta.dialects.Scala34

class SourceMergerIncrementalSuite extends BaseSuite {
  val merger = SourceMerger()

  test("preserve implemented typed bindings when generated annotations change") {
    val original = source"""
      def run = for {
        user: User <- loadUser()
        label: Label = customLabel(user)
      } yield label
    """
    val generated = source"""
      def run = for {
        user: NewUser <- defaultUser()
        label: NewLabel = defaultLabel(user)
        saved <- save(user)
      } yield label
    """
    val expected = source"""
      def run = for {
        user: User <- loadUser()
        label: Label = customLabel(user)
        saved <- save(user)
      } yield label
    """
    val result = merger.merge(original, generated)
    assertEqStructure(result, expected)
    assertEqStructure(merger.merge(result.parse[Source].get, generated), expected)
  }

  test("preserve implemented response qualifiers when regenerating http4s routes") {
    val original = source"""
      def routes = HttpRoutes.of[IO] {
        case req @ POST -> Root / "users" =>
          for {
            reqBody <- req.as[User]
            saved <- save(reqBody)
            resp <- Created(saved)
          } yield resp
      }
    """
    val generated = source"""
      def routes = HttpRoutes.of[IO] {
        case req @ POST -> Root / "users" =>
          for {
            reqBody <- req.as[NewUser]
            resp <- NotImplemented("TODO")
          } yield resp
        case req @ GET -> Root / "users" =>
          for (resp <- NotImplemented("TODO list")) yield resp
      }
    """
    val expected = source"""
      def routes = HttpRoutes.of[IO] {
        case req @ POST -> Root / "users" =>
          for {
            reqBody <- req.as[NewUser]
            saved <- save(reqBody)
            resp <- Created(saved)
          } yield resp
        case req @ GET -> Root / "users" =>
          for (resp <- NotImplemented("TODO list")) yield resp
      }
    """
    val result = merger.merge(original, generated)
    assertEqStructure(result, expected)
    assertEqStructure(merger.merge(result.parse[Source].get, generated), expected)
  }

  test("update generated parent type arguments while preserving user parents and object members") {
    val original = source"""
      object Age extends RefinedType[Int, GreaterEqual[0]] with UserMarker {
        def description = "user age"
      }
    """
    val generated = source"""
      object Age extends RefinedType[Int, GreaterEqual[18]]
    """
    val expected = source"""
      object Age extends RefinedType[Int, GreaterEqual[18]] with UserMarker {
        def description = "user age"
      }
    """
    val result = merger.merge(original, generated)
    assertEqStructure(result, expected)
    assertEqStructure(merger.merge(result.parse[Source].get, generated), expected)
  }

  test("preserve implemented value bindings without dropping newly generated wildcard effects") {
    val original = source"""
      def run = for {
        user <- loadUser()
        label = customLabel(user)
        _ <- audit(user)
      } yield label
    """
    val generated = source"""
      def run = for {
        user <- loadUser()
        label = defaultLabel(user)
        _ <- authorize(user)
      } yield label
    """
    val expected = source"""
      def run = for {
        user <- loadUser()
        label = customLabel(user)
        _ <- audit(user)
        _ <- authorize(user)
      } yield label
    """
    assertEqStructure(merger.merge(original, generated), expected)
  }

  test("update matching parent types of classes and traits without replacing user constructor arguments") {
    val original = source"""
      class Handler extends Base[OldModel](userConfig) { def handle = "keep" }
      trait HandlerApi extends Api[OldModel] { def custom = "keep" }
    """
    val generated = source"""
      class Handler extends Base[NewModel](defaultConfig)
      trait HandlerApi extends Api[NewModel]
    """
    val expected = source"""
      class Handler extends Base[NewModel](userConfig) { def handle = "keep" }
      trait HandlerApi extends Api[NewModel] { def custom = "keep" }
    """
    assertEqStructure(merger.merge(original, generated), expected)
  }
}
