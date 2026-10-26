package ba.sake.regenesca

import scala.meta._

/** Renders for-comprehension qualifiers on separate lines inside Scala trees. */
object ForComprehensionPrinter {
  private case class Edit(start: Int, end: Int, replacement: String)

  def syntax(tree: Tree)(implicit dialect: Dialect): String = {
    val printed = tree.syntax
    val parsed = tree match {
      case _: Source => printed.parse[Source].get
      case _: Term   => printed.parse[Term].get
      case _: Stat   => printed.parse[Stat].get
      case _         => return printed
    }

    val edits = parsed.collect {
      case term: Term.For      => qualifierEdits(term.enumsBlock, term.pos.startColumn)
      case term: Term.ForYield => qualifierEdits(term.enumsBlock, term.pos.startColumn)
    }.flatten

    edits.sortBy(_.start).reverse.foldLeft(printed) { (result, edit) =>
      result.substring(0, edit.start) + edit.replacement + result.substring(edit.end)
    }
  }

  private def qualifierEdits(block: Term.EnumeratorsBlock, column: Int): List[Edit] = {
    if (block.tokens.exists {
          case _: Token.Comment => true
          case _                => false
        }) return Nil

    val qualifiers = block.enums
    val indentation = " " * column
    val qualifierIndentation = indentation + "  "
    val opening = Edit(block.pos.start, qualifiers.head.pos.start, "{\n" + qualifierIndentation)
    val separators = qualifiers.sliding(2).collect { case List(left, right) =>
      Edit(left.pos.end, right.pos.start, "\n" + qualifierIndentation)
    }.toList
    val closing = Edit(qualifiers.last.pos.end, block.pos.end, "\n" + indentation + "}")
    opening :: separators ::: List(closing)
  }
}
