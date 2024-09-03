package ba.sake.regenesca

import java.nio.file.Path
import scala.meta.Source

case class GeneratedFileSource(
    file: Path,
    source: Source
)
