package ba.sake.regenesca

import java.nio.file.Path

case class GeneratedFilePreview(
    file: Path,
    existed: Boolean,
    changed: Boolean,
    mergedSource: String
)
