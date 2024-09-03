package example

import java.nio.file._
import scala.meta._
import scala.meta.dialects.Scala34
import ba.sake.regenesca._

object Example extends App {
  val merger = SourceMerger()
  val generator = RegenescaGenerator(merger)

  val sqlTables = Seq("user", "item", "customer")
  val generatedSources = sqlTables.map { sqlTable =>
    val filePath =
      Paths.get(s"example/src/generated/${sqlTable.capitalize}.scala")
    GeneratedFileSource(filePath, generateSource(sqlTable))
  }
  generator.generate(generatedSources)

  def generateSource(sqlTable: String): Source = {
    val typeName = Type.Name(sqlTable.capitalize)
    val termName = Term.Name(sqlTable.capitalize)
    val tableNameLit = Lit.String(sqlTable)
    source"""
    case class ${typeName}()

    object ${termName} {
      val tableName = ${tableNameLit}
    }
    """
  }
}
