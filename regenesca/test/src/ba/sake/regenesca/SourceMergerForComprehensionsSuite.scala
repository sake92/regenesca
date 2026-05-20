package ba.sake.regenesca

import scala.meta._
import scala.meta.dialects.Scala34

class SourceMergerForComprehensionsSuite extends BaseSuite {

  val sourceMerger = SourceMerger()

  test("should merge for-yield qualifiers and preserve user yield expression") {
    val original = source"""
      def route = {
        for {
          req <- fetchReq("v1")
          user <- decodeUser(req)
        } yield user.withAudit("keep-user-edit")
      }
    """
    val generated = source"""
      def route = {
        for {
          auth <- authenticate()
          req <- fetchReq("v2")
          user <- decodeUser(req)
          if user.isActive
        } yield user
      }
    """
    val expected = source"""
      def route = {
        for {
          req <- fetchReq("v2")
          user <- decodeUser(req)
          auth <- authenticate()
          if user.isActive
        } yield user.withAudit("keep-user-edit")
      }
    """
    val merged = sourceMerger.merge(original, generated)
    assertEqStructure(merged, expected)
  }

  test("should merge nested for-comprehensions") {
    val original = source"""
      def nested = {
        for {
          a <- outerSource("v1")
        } yield {
          for {
            b <- innerSource("old")
          } yield b
        }
      }
    """
    val generated = source"""
      def nested = {
        for {
          a <- outerSource("v2")
          if a.nonEmpty
        } yield {
          for {
            b <- innerSource("new")
            c <- enrich(b)
          } yield c
        }
      }
    """
    val expected = source"""
      def nested = {
        for {
          a <- outerSource("v2")
          if a.nonEmpty
        } yield {
          for {
            b <- innerSource("new")
            c <- enrich(b)
          } yield b
        }
      }
    """
    val merged = sourceMerger.merge(original, generated)
    assertEqStructure(merged, expected)
  }

  test("should support http4s-style route case bodies") {
    val original = source"""
      def routes = HttpRoutes.of[IO] {
        case GET -> Root / "users" =>
          for {
            req <- reqDecoder("v1")
            user <- findUser(req)
          } yield toResponse(user, "keep-manual")
      }
    """
    val generated = source"""
      def routes = HttpRoutes.of[IO] {
        case GET -> Root / "users" =>
          for {
            trace <- traceId()
            req <- reqDecoder("v2")
            user <- findUser(req)
            if user.allowed
          } yield toResponse(user)
      }
    """
    val expected = source"""
      def routes = HttpRoutes.of[IO] {
        case GET -> Root / "users" =>
          for {
            req <- reqDecoder("v2")
            user <- findUser(req)
            trace <- traceId()
            if user.allowed
          } yield toResponse(user, "keep-manual")
      }
    """
    val merged = sourceMerger.merge(original, generated)
    assertEqStructure(merged, expected)
  }

  test("should be idempotent when merging for-comprehensions repeatedly") {
    val original = source"""
      def route = {
        for {
          a <- sourceA("1")
        } yield a
      }
    """
    val generated = source"""
      def route = {
        for {
          a <- sourceA("2")
          b <- sourceB(a)
        } yield b
      }
    """
    val once = sourceMerger.merge(original, generated)
    val twice = sourceMerger.merge(once.parse[Source].get, generated)
    assertEqStructure(once, once.parse[Source].get)
    assertEqStructure(twice, once.parse[Source].get)
  }

  test("should overwrite comprehension fully when configured") {
    val merger = SourceMerger(
      forComprehensionMergeStrategy = ForComprehensionMergeStrategy.OverwriteComprehensionFully
    )
    val original = source"""
      def route = {
        for {
          oldStep <- oldSource()
          req <- fetchReq("v1")
        } yield req.keepUserEdit
      }
    """
    val generated = source"""
      def route = {
        for {
          req <- fetchReq("v2")
          if req.nonEmpty
        } yield req
      }
    """
    val merged = merger.merge(original, generated)
    assertEqStructure(merged, generated)
  }

  test("should match defs by signature and keep overloads safe") {
    val original = source"""
      class Api {
        def load(id: Int): String = "old-int"
        def load(id: String): String = "old-string"
      }
    """
    val generated = source"""
      class Api {
        def load(id: Int): String = "old-int"
        def load(id: String): String = "new-string"
      }
    """
    val expected = source"""
      class Api {
        def load(id: Int): String = "old-int"
        def load(id: String): String = "new-string"
      }
    """
    val merged = sourceMerger.merge(original, generated)
    assertEqStructure(merged, expected)
  }

  test("should match overloaded defs by signature when mergeDefBodies is false") {
    val merger = SourceMerger(mergeDefBodies = false)
    val original = source"""
      class Api {
        def load(id: Int): String = "old-int"
        def load(id: String): String = "old-string"
      }
    """
    val generated = source"""
      class Api {
        def load(id: Int): String = "new-int"
        def load(id: String): String = "old-string"
      }
    """
    val expected = source"""
      class Api {
        def load(id: Int): String = "new-int"
        def load(id: String): String = "old-string"
      }
    """
    val merged = merger.merge(original, generated)
    assertEqStructure(merged, expected)
  }
}
