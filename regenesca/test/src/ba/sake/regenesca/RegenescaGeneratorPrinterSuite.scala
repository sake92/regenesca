package ba.sake.regenesca

import java.nio.file.Files
import scala.meta._
import scala.meta.dialects.Scala34

class RegenescaGeneratorPrinterSuite extends BaseSuite {
  test("prints qualifiers on separate lines in a newly generated file") {
    val directory = Files.createTempDirectory("regenesca-printer-")
    val file = directory.resolve("Generated.scala")
    val source = "def route = for (a <- as; b <- bs(a)) yield b".parse[Source].get

    try {
      RegenescaGenerator(SourceMerger()).generate(Seq(GeneratedFileSource(file, source)))

      assertEquals(
        Files.readString(file),
        """def route = for {
          |              a <- as
          |              b <- bs(a)
          |            } yield b""".stripMargin
      )
    } finally {
      Files.deleteIfExists(file)
      Files.deleteIfExists(directory)
    }
  }
}
