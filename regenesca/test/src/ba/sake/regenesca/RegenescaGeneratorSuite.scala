package ba.sake.regenesca

import java.nio.file.Files
import scala.meta._
import scala.meta.dialects.Scala34

class RegenescaGeneratorSuite extends munit.FunSuite {

  test("generatePreview should report new file without writing") {
    val root = Files.createTempDirectory("regenesca-preview")
    val file = root.resolve("a").resolve("Test.scala")
    val generator = RegenescaGenerator(SourceMerger())
    val preview = generator.generatePreview(Seq(GeneratedFileSource(file, source"class Test")))
    assertEquals(preview.size, 1)
    assertEquals(preview.head.existed, false)
    assertEquals(preview.head.changed, true)
    assertEquals(preview.head.mergedSource.trim, "class Test")
    assertEquals(Files.exists(file), false)
  }

  test("generatePreview should include file path context on parse errors") {
    val root = Files.createTempDirectory("regenesca-preview-parse")
    val file = root.resolve("Broken.scala")
    Files.writeString(file, "def x = ")
    val generator = RegenescaGenerator(SourceMerger())
    val ex = intercept[IllegalArgumentException] {
      generator.generatePreview(Seq(GeneratedFileSource(file, source"class Test")))
    }
    assert(clue(ex.getMessage).contains("Failed to parse source"))
    assert(clue(ex.getMessage).contains(file.toAbsolutePath.toString))
  }
}
