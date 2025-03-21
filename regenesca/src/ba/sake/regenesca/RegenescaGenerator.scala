package ba.sake.regenesca

import java.nio.file._
import scala.meta._

class RegenescaGenerator(merger: SourceMerger) {

  def generate(
      generatedFileSources: Seq[GeneratedFileSource]
  )(implicit dialect: Dialect): Unit =
    generatedFileSources.foreach { generatedFileSource =>
      val filePath = generatedFileSource.file
      Files.createDirectories(filePath.getParent)
      if (Files.exists(filePath)) {
        val fileSource = readFileSource(filePath)
        val regeneratedFileSource = merger.merge(fileSource, generatedFileSource.source)
        Files.writeString(filePath, regeneratedFileSource)
      } else {
        Files.writeString(filePath, generatedFileSource.source.syntax)
      }
    }

  private def readFileSource(filePath: Path)(implicit dialect: Dialect): Source = {
    val bytes = Files.readAllBytes(filePath)
    val text = new String(bytes, "UTF-8")
    val input = Input.VirtualFile(filePath.toString, text)
    input.parse[Source].get
  }
}

object RegenescaGenerator {
  def apply(merger: SourceMerger): RegenescaGenerator =
    new RegenescaGenerator(merger)
}
