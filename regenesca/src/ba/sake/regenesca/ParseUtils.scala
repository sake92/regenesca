package ba.sake.regenesca

import scala.meta._

object ParseUtils {

  def parseSourceOrThrow(input: Input, context: String): Source =
    input.parse[Source] match {
      case parsers.Parsed.Success(source) => source
      case parsers.Parsed.Error(pos, message, details) =>
        throw new IllegalArgumentException(
          s"[regenesca] Failed to parse source ($context) at ${pos.startLine + 1}:${pos.startColumn + 1}: $message. $details"
        )
    }
}
