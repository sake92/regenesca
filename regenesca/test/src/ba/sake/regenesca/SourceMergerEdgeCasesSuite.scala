package ba.sake.regenesca

import scala.meta._
import scala.meta.dialects.Scala34

class SourceMergerEdgeCasesSuite extends BaseSuite {

  val sourceMerger = SourceMerger()

  test("should fail on duplicate generated val merge keys") {
    val original = source"""
      val a = 1
    """
    val generated = source"""
      val (a, b) = pair1
      val (a, c) = pair2
    """
    val ex = intercept[IllegalArgumentException] {
      sourceMerger.merge(original, generated)
    }
    assert(clue(ex.getMessage).contains("Ambiguous merge keys"))
    assert(clue(ex.getMessage).contains("val names/patterns"))
  }

  test("should not merge cases when guard/body-shape key differs") {
    val original = source"""
      def routes = Routes {
        case GET() -> Path("users") if legacyGuard =>
          Response.withStatus(200).withBody("keep-old-body")
      }
    """
    val generated = source"""
      def routes = Routes {
        case GET() -> Path("users") if currentGuard =>
          Response.withStatus(200)
      }
    """
    val expected = source"""
      def routes = Routes {
        case GET() -> Path("users") if legacyGuard =>
          Response.withStatus(200).withBody("keep-old-body")
        case GET() -> Path("users") if currentGuard =>
          Response.withStatus(200)
      }
    """
    val merged = sourceMerger.merge(original, generated)
    assertEqStructure(merged, expected)
  }

  test("should match structural terms by key when generated block order changes") {
    val original = source"""
      def route = {
        for {
          a <- sourceA("v1")
        } yield a.keepUserA
        for {
          b <- sourceB("v1")
        } yield b.keepUserB
      }
    """
    val generated = source"""
      def route = {
        for {
          b <- sourceB("v2")
          c <- enrichB(b)
        } yield c
        for {
          a <- sourceA("v2")
        } yield a
      }
    """
    val expected = source"""
      def route = {
        for {
          a <- sourceA("v2")
        } yield a.keepUserA
        for {
          b <- sourceB("v2")
          c <- enrichB(b)
        } yield b.keepUserB
      }
    """
    val merged = sourceMerger.merge(original, generated)
    assertEqStructure(merged, expected)
  }

  test("should support overwriting destructured val patterns") {
    val original = source"""
      val (a, b) = oldPair
      val untouched = "x"
    """
    val generated = source"""
      val (a, b) = newPair
    """
    val expected = source"""
      val (a, b) = newPair
      val untouched = "x"
    """
    val merged = sourceMerger.merge(original, generated)
    assertEqStructure(merged, expected)
  }
}
