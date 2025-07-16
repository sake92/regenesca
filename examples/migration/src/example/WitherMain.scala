package example

import java.nio.file._
import scala.meta._
import scala.meta.dialects.Scala34
import ba.sake.regenesca._

object WitherMain extends App {

  val filePath = Paths.get("examples/migration/src/refactor/MyClass.scala")
  val source = readFileSource(filePath)

  val transformedSource = source
    .transform { case cls: Defn.Class =>
      val hasWither = cls.mods.exists {
        case mod: Mod.Annot => mod.text == "@Wither"
        case _              => false
      }
      if (hasWither) {
        val newWithers = makeWithers(cls.ctor.paramClauses.head.values, cls.templ.body.stats, cls.name)
        val transformedBody = cls.templ.body.copy(
          stats = cls.templ.body.stats ++ newWithers
        )
        cls.copy(
          templ = cls.templ.copy(
            earlyClause = cls.templ.earlyClause,
            inits = cls.templ.inits,
            body = transformedBody,
            derives = cls.templ.derives
          )
        )
      } else {
        cls
      }
    }
    .asInstanceOf[Source]

  val generator = RegenescaGenerator(SourceMerger())
  val generatedSources = Seq(GeneratedFileSource(filePath, transformedSource))
  generator.generate(generatedSources)

  private def readFileSource(filePath: Path): Source = {
    val bytes = Files.readAllBytes(filePath)
    val text = new String(bytes, "UTF-8")
    val input = Input.VirtualFile(filePath.toString, text)
    input.parse[Source].get
  }

  private def makeWithers(params: Seq[Term.Param], classStats: List[Stat], className: Type.Name): Seq[Stat] = {
    val existingWitherNames = classStats.collect {
      case defn: Defn.Def if defn.name.value.startsWith("with") => defn.name.value
    }.toSet
    params.flatMap { param =>
      val paramName = param.name
      val paramType = param.decltpe.get
      val newWitherName = s"with${paramName.value.capitalize}"
      Option.when(!existingWitherNames(newWitherName)) {
        q"""
        def ${Term.Name(newWitherName)}(${paramName}: ${paramType}): ${className} = 
          copy(${Term.Name(paramName.value)} = ${Term.Name(paramName.value)})
        """
      }
    }
  }
}
