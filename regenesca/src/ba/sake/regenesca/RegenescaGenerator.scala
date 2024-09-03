package ba.sake.regenesca

import java.nio.file._
import scala.meta._
import ba.sake.regenesca.SourceMerger

class RegenescaGenerator(merger: SourceMerger) {

  def generate(
      generatedFileSources: Seq[GeneratedFileSource]
  )(implicit dialect: Dialect): Unit =
    generatedFileSources.foreach { generatedFileSource =>
      val filePath = generatedFileSource.file
      val fileSource = readFileSourceOrEmpty(filePath)
      val regeneratedFileSource =
        merger.merge(fileSource, generatedFileSource.source)
      Files.createDirectories(filePath.getParent())
      // TODO scalafmt pretty??
      Files.writeString(filePath, regeneratedFileSource.syntax)
    }

  private def readFileSourceOrEmpty(
      filePath: Path
  )(implicit dialect: Dialect): Source =
    if (Files.exists(filePath)) {
      val bytes = Files.readAllBytes(filePath)
      val text = new String(bytes, "UTF-8")
      val input = Input.VirtualFile(filePath.toString, text)
      input.parse[Source].get
    } else {
      Source(List.empty)
    }
}

object RegenescaGenerator {
  def apply(merger: SourceMerger): RegenescaGenerator =
    new RegenescaGenerator(merger)
}
