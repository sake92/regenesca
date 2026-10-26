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
      case term: Term.For      => qualifierEdits(term)
      case term: Term.ForYield => qualifierEdits(term)
    }.flatten

    edits.sortBy(_.start).reverse.foldLeft(printed) { (result, edit) =>
      result.substring(0, edit.start) + edit.replacement + result.substring(edit.end)
    }
  }

  private def qualifierEdits(term: Term): List[Edit] = {
    val (block, bodyStart) = term match {
      case forTerm: Term.For      => forTerm.enumsBlock -> forTerm.body.pos.start
      case forYield: Term.ForYield => forYield.enumsBlock -> forYield.body.pos.start
      case _ => return Nil
    }
    if (block.tokens.exists {
          case _: Token.Comment => true
          case _                => false
        }) return Nil

    val qualifiers = block.enums
    val indentation = " " * term.pos.startColumn
    val qualifierIndentation = indentation + "  "
    val opening = Edit(term.tokens.head.pos.end, qualifiers.head.pos.start, " {\n" + qualifierIndentation)
    val separators = qualifiers.sliding(2).collect { case List(left, right) =>
      Edit(left.pos.end, right.pos.start, "\n" + qualifierIndentation)
    }.toList
    val closingEnd = term match {
      case _: Term.ForYield =>
        term.tokens.filter(_.text == "yield").filter(_.pos.end <= bodyStart).last.pos.start
      case _ => block.pos.end
    }
    val closing = Edit(qualifiers.last.pos.end, closingEnd, "\n" + indentation + "} ")
    opening :: separators ::: List(closing)
  }
}
