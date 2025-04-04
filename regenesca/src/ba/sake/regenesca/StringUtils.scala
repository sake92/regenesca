package ba.sake.regenesca

object StringUtils {

  // java has it but from java 12..
  def indent(str: String, spaces: Int): String = {
    val indentSpaces = " " * spaces
    str
      .split("\n")
      .map { line =>
        indentSpaces + line
      }
      .mkString("\n")
  }
}
