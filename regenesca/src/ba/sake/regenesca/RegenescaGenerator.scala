package ba.sake.regenesca

import java.nio.file._
import scala.meta._

class RegenescaGenerator(merger: SourceMerger)(implicit dialect: Dialect) {

  def generate(generatedFileSources: Seq[GeneratedFileSource]): Unit =
    generatePreview(generatedFileSources).foreach { preview =>
      Files.createDirectories(preview.file.getParent)
      Files.writeString(preview.file, preview.mergedSource)
    }

  def generatePreview(generatedFileSources: Seq[GeneratedFileSource]): Seq[GeneratedFilePreview] =
    generatedFileSources.map { generatedFileSource =>
      val filePath = generatedFileSource.file
      if (Files.exists(filePath)) {
        val fileSource = readFileSource(filePath)
        val mergedSource = merger.merge(fileSource, generatedFileSource.source)
        GeneratedFilePreview(
          file = filePath,
          existed = true,
          changed = mergedSource != fileSource.syntax,
          mergedSource = mergedSource
        )
      } else {
        GeneratedFilePreview(
          file = filePath,
          existed = false,
          changed = true,
          mergedSource = generatedFileSource.source.syntax
        )
      }
    }

  private def readFileSource(filePath: Path): Source = {
    val bytes = Files.readAllBytes(filePath)
    val text = new String(bytes, "UTF-8")
    val input = Input.VirtualFile(filePath.toString, text)
    ParseUtils.parseSourceOrThrow(input, s"file=${filePath.toAbsolutePath}")
  }
}

object RegenescaGenerator {
  def apply(merger: SourceMerger)(implicit dialect: Dialect): RegenescaGenerator =
    new RegenescaGenerator(merger)
}
