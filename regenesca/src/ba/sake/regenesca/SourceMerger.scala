package ba.sake.regenesca

import scala.meta._
import scala.meta.contrib._
import scalafix.patch._
import scalafix.internal.patch._

import scala.annotation.tailrec
import scala.meta.Stat.Block

sealed trait ForComprehensionMergeStrategy
object ForComprehensionMergeStrategy {
  case object PreserveUserExpressions extends ForComprehensionMergeStrategy
  case object OverwriteComprehensionFully extends ForComprehensionMergeStrategy
}

class SourceMerger(
    mergeDefBodies: Boolean,
    forComprehensionMergeStrategy: ForComprehensionMergeStrategy
)(implicit dialect: Dialect) {

  def merge(originalSource: Source, overwriteSource: Source): String =
    if (originalSource.stats.isEmpty) {
      overwriteSource.syntax
    } else {
      val origSourceLastToken = originalSource.tokens.last
      val patches =
        patchStats(origSourceLastToken, false, originalSource.stats, overwriteSource.stats)
      // println(s"GENERATED PATCHES: ${patches.mkString("\n")}")
      val ctx = scalafix.v0.RuleCtx(originalSource)
      PatchInternals.tokenPatchApply(ctx, None, patches)
    }

  // TODO rename
  private def patchStats(
      origSourceLastToken: Token,
      hasBody: Boolean,
      originalStats: List[Stat],
      generatedStats: List[Stat],
      appendNewDefinitions: Boolean = true
  ): List[Patch] = {
    // dont consider new expressions at all!
    val overwritingStats = generatedStats.filterNot(_.isInstanceOf[Term])
    var usedOverwritingStats: Set[Stat] = Set.empty
    // map keys are just strings!
    val overwritingPkgsMap = overwritingStats.collect { case p2: Pkg =>
      p2.name.value -> p2
    }.toMap
    val overwritingImports = overwritingStats.collect { case i2: Import =>
      i2
    }
    val overwritingValsMap = overwritingStats
      .collect { case v2: Defn.Val => v2 }
      .flatMap { v =>
        v.pats.headOption.collect { case p: Pat.Var =>
          p.name.value -> v
        }
      }
      .toMap
    val overwritingVarsMap = overwritingStats
      .collect { case v2: Defn.Var => v2 }
      .flatMap { v =>
        v.pats.headOption.collect { case p: Pat.Var =>
          p.name.value -> v
        }
      }
      .toMap
    val overwritingDefsMap = overwritingStats.collect { case d2: Defn.Def =>
      d2
    }
    val overwritingDefsBySignatureMap = toUniqueMap(overwritingDefsMap, defSignatureKey, "def signatures")
    val overwritingEnumsMap = overwritingStats.collect { case e2: Defn.Enum =>
      e2.name.value -> e2
    }.toMap
    val overwritingClassesMap = overwritingStats.collect { case c2: Defn.Class =>
      c2.name.value -> c2
    }.toMap
    val overwritingTraitsMap = overwritingStats.collect { case t2: Defn.Trait =>
      t2.name.value -> t2
    }.toMap
    val overwritingObjectsMap = overwritingStats.collect { case o2: Defn.Object =>
      o2.name.value -> o2
    }.toMap
    val overwritingTypesMap = overwritingStats.collect { case t2: Defn.Type =>
      t2.name.value -> t2
    }.toMap
    val overwritingGivensMap = overwritingStats.collect { case g2: Defn.Given =>
      // try to encode a "name" for anonymous givens...
      val key = g2.name + g2.templ.inits.map(_.structure).mkString("-")
      key -> g2
    }.toMap
    val overwritingGivenAliasesMap = overwritingStats.collect { case g2: Defn.GivenAlias =>
      g2.decltpe.structure -> g2
    }.toMap

    /* do the merging */
    val patchesList: List[Patch] = originalStats.flatMap {
      case p1: Pkg =>
        overwritingPkgsMap.get(p1.name.value) match {
          case Some(p2) =>
            usedOverwritingStats += p2
            patchStats(origSourceLastToken, hasBody, p1.stats, p2.stats)
          case None =>
            List.empty
        }
      case i1: Import =>
        overwritingImports.find(_.isEqual(i1)) match {
          case Some(i2) =>
            usedOverwritingStats += i2
            List.empty
          case None => List.empty
        }
      case v1: Defn.Val =>
        val v1Name = v1.pats.headOption
          .collect { case p: Pat.Var =>
            p.name.value
          }
          .getOrElse("")
        overwritingValsMap.get(v1Name) match {
          case Some(v2) =>
            usedOverwritingStats += v2
            Option.when(v1.structure != v2.structure)(Patch.replaceTree(v1, v2.syntax)).toList
          case None =>
            List.empty
        }
      case v1: Defn.Var =>
        val v1Name = v1.pats.headOption
          .collect { case p: Pat.Var =>
            p.name.value
          }
          .getOrElse("")
        overwritingVarsMap.get(v1Name) match {
          case Some(v2) =>
            usedOverwritingStats += v2
            Option.when(v1.structure != v2.structure)(Patch.replaceTree(v1, v2.syntax)).toList
          case None =>
            List.empty
        }
      case d1: Defn.Def =>
        overwritingDefsBySignatureMap.get(defSignatureKey(d1)) match {
          case Some(d2) =>
            usedOverwritingStats += d2
            if (mergeDefBodies) {
              patchTerms(d1.tokens.last, d1.body.tokens.nonEmpty, d1.body, d2.body)
            } else {
              Option.when(d1.structure != d2.structure)(Patch.replaceTree(d1, d2.syntax)).toList
            }
          case None =>
            List.empty
        }
      case e1: Defn.Enum =>
        overwritingEnumsMap.get(e1.name.value) match {
          case Some(e2) =>
            usedOverwritingStats += e2
            Option.when(e1.structure != e2.structure)(Patch.replaceTree(e1, e2.syntax)).toList
          case None =>
            List.empty
        }
      case c1: Defn.Class =>
        overwritingClassesMap.get(c1.name.value) match {
          case Some(c2) =>
            usedOverwritingStats += c2
            var usedOverwritingParamClauses: Set[Term.ParamClause] = Set.empty
            val overwrittenParamClauses =
              c1.ctor.paramClauses.zipWithIndex.flatMap { case (paramClause1, i) =>
                c2.ctor.paramClauses.lift(i) match {
                  case Some(paramClause2) =>
                    usedOverwritingParamClauses += paramClause2
                    val overwritingParamsMap = paramClause2.values
                      .map(p2 => p2.name.value -> p2)
                      .toMap
                    var usedOverwritingParams: Set[Term.Param] = Set.empty
                    val overwriteParamPatches = paramClause1.values.flatMap { param1 =>
                      overwritingParamsMap.get(param1.name.value) match {
                        case Some(overwritingParam) =>
                          usedOverwritingParams += overwritingParam
                          Option
                            .when(param1.structure != overwritingParam.structure)(
                              Patch.replaceTree(param1, overwritingParam.syntax)
                            )
                            .toList
                        case None =>
                          List.empty
                      }
                    }
                    val newParams = paramClause2.values.filterNot(usedOverwritingParams)
                    val newParamPatches = if (paramClause1.values.isEmpty) newParams.zipWithIndex.map { case (p2, i) =>
                      val prefix = if (i == 0) "" else ", "
                      Patch.addLeft(paramClause1.tokens.last, s"${prefix}${p2.syntax}")
                    }
                    else newParams.map(p2 => Patch.addRight(paramClause1.values.last, s", ${p2.syntax}"))
                    overwriteParamPatches ++ newParamPatches
                  case None =>
                    List.empty
                }
              }
            val newParamClauses = c2.ctor.paramClauses.filterNot(usedOverwritingParamClauses)
            val paramClauses = overwrittenParamClauses ++ newParamClauses.map { paramClause2 =>
              Patch.addRight(c1.ctor.paramClauses.last.tokens.last, s"${paramClause2.syntax}")
            }
            val mergedTemplStats =
              patchStats(c1.tokens.last, c1.templ.body.tokens.nonEmpty, c1.templ.stats, c2.templ.stats)
            val modsPatches = c2.ctor.mods.map { m2 =>
              if (c1.ctor.mods.contains(m2)) Patch.empty
              else if (c1.ctor.mods.isEmpty) Patch.addLeft(c1.ctor.paramClauses.head.tokens.head, s" ${m2.syntax}")
              else Patch.addRight(c1.ctor.mods.last.tokens.last, s" ${m2.syntax}")
            }
            mergedTemplStats ++ paramClauses ++ modsPatches
          case None =>
            List.empty
        }
      case t1: Defn.Trait =>
        overwritingTraitsMap.get(t1.name.value) match {
          case Some(t2) =>
            usedOverwritingStats += t2
            patchStats(t1.tokens.last, t1.templ.body.tokens.nonEmpty, t1.templ.stats, t2.templ.stats)
          case None =>
            List.empty
        }
      case o1: Defn.Object =>
        overwritingObjectsMap.get(o1.name.value) match {
          case Some(o2) =>
            usedOverwritingStats += o2
            patchStats(o1.tokens.last, o1.templ.body.tokens.nonEmpty, o1.templ.stats, o2.templ.stats)
          case None =>
            List.empty
        }
      case t1: Defn.Type =>
        overwritingTypesMap.get(t1.name.value) match {
          case Some(t2) =>
            usedOverwritingStats += t2
            Option.when(t1.structure != t2.structure)(Patch.replaceTree(t1, t2.syntax)).toList
          case None =>
            List.empty
        }
      case g1: Defn.Given =>
        // try to encode a "name" for anonymous givens...
        val key = g1.name + g1.templ.inits.map(_.structure).mkString("-")
        overwritingGivensMap.get(key) match {
          case Some(g2) =>
            usedOverwritingStats += g2
            Option.when(g1.structure != g2.structure)(Patch.replaceTree(g1, g2.syntax)).toList
          case None =>
            List.empty
        }
      case g1: Defn.GivenAlias =>
        overwritingGivenAliasesMap.get(g1.decltpe.structure) match {
          case Some(g2) =>
            usedOverwritingStats += g2
            Option.when(g1.structure != g2.structure)(Patch.replaceTree(g1, g2.syntax)).toList
          case None =>
            List.empty
        }
      // leave other statements intact
      case _ =>
        List.empty
    }

    val patches = patchesList.toBuffer

    /* insert new stats at appropriate position */
    val newStats = overwritingStats.filterNot(usedOverwritingStats)
    locally {
      val newImports = newStats.collect { case i2: Import => i2 }
      usedOverwritingStats ++= newImports
      patches ++= newImports.flatMap(_.importers.map(i => Patch.addGlobalImport(i)))
    }
    locally {
      val newVals = newStats.collect { case v2: Defn.Val => v2 }
      if (newVals.nonEmpty) {
        usedOverwritingStats ++= newVals
        val lastValVar =
          originalStats
            .findLast(s => s.isInstanceOf[Defn.Val] || s.isInstanceOf[Defn.Var])
            .getOrElse(originalStats.last)
        // try to keep existing indentation...
        patches ++= newVals.map { v =>
          val indentedVal = StringUtils.indent(v.syntax, lastValVar.pos.startColumn)
          Patch.addRight(lastValVar, "\n" + indentedVal)
        }
      }
    }
    locally {
      val newVars = newStats.collect { case v2: Defn.Var => v2 }
      if (newVars.nonEmpty) {
        usedOverwritingStats ++= newVars
        val lastValVar =
          originalStats
            .findLast(s => s.isInstanceOf[Defn.Val] || s.isInstanceOf[Defn.Var])
            .getOrElse(originalStats.last)
        patches ++= newVars.map { v =>
          val indentedVar = StringUtils.indent(v.syntax, lastValVar.pos.startColumn)
          Patch.addRight(lastValVar, "\n" + indentedVar)
        }
      }
    }
    locally {
      val otherStats = overwritingStats.filterNot(usedOverwritingStats)
      if (otherStats.nonEmpty) {
        val newPatches = if (appendNewDefinitions) {
          val afterStat = originalStats.lastOption
          afterStat match {
            case Some(after) =>
              // println(s"INSERTING after $afterStat OTHER STATS: ${otherStats}")
              otherStats.map { s =>
                val indentedStat = StringUtils.indent(s.syntax, after.pos.startColumn)
                Patch.addRight(after, "\n" + indentedStat)
              }
            case None =>
              if (hasBody) {
                otherStats.map { s =>
                  Patch.addLeft(origSourceLastToken, "\n" + s.syntax)
                }
              } else {
                val newBlock = Block(otherStats)
                Seq(Patch.addRight(origSourceLastToken, newBlock.syntax))
              }
          }
        } else {
          // in a block
          val beforeStat = originalStats.head
          // println(s"INSERTING before $beforeStat OTHER STATS: ${otherStats}")
          otherStats.map { s =>
            val spaces = " " * beforeStat.pos.startColumn
            Patch.addLeft(beforeStat, s"${s.syntax}\n" + spaces)
          }
        }
        patches ++= newPatches
      }
    }
    patches.toList
  }

  @tailrec
  private def patchTerms(
      origSourceLastToken: Token,
      hasBody: Boolean,
      originalTerm: Term,
      overwriteTerm: Term
  ): List[Patch] =
    (originalTerm, overwriteTerm) match {
      case (t1: Term.Block, t2: Term.Block) =>
        patchStats(origSourceLastToken, hasBody, t1.stats, t2.stats, appendNewDefinitions = false)
      case (t1: Term.Apply, t2: Term.Apply) =>
        if ( // only handling one-arg functions...
          t1.args.length == 1 && t2.args.length == 1 &&
          t1.fun.isInstanceOf[Term.Name] &&
          t2.fun.isInstanceOf[Term.Name] &&
          t1.fun.asInstanceOf[Term.Name].value ==
            t2.fun.asInstanceOf[Term.Name].value
        ) {
          patchTerms(origSourceLastToken, hasBody, t1.argClause.values.head, t2.argClause.values.head)
        } else {
          List.empty
        }
      case (t1: Term.Apply, t2: Term.Block) =>
        // if it's just an expression like Response.withBody("")
        // and we add a block
        // just treat that expr as a block and merge them
        patchTerms(origSourceLastToken, hasBody, Term.Block(List(t1)), t2)
      case (t1: Term.PartialFunction, t2: Term.PartialFunction) =>
        patchCases(origSourceLastToken, hasBody, t1.cases, t2.cases)
      case (t1: Term.For, t2: Term.For) =>
        forComprehensionMergeStrategy match {
          case ForComprehensionMergeStrategy.OverwriteComprehensionFully =>
            Option.when(t1.structure != t2.structure)(Patch.replaceTree(t1, t2.syntax)).toList
          case ForComprehensionMergeStrategy.PreserveUserExpressions =>
            val merged = mergeForTerm(t1, t2)
            Option.when(merged.structure != t1.structure)(Patch.replaceTree(t1, merged.syntax)).toList
        }
      case (t1: Term.ForYield, t2: Term.ForYield) =>
        forComprehensionMergeStrategy match {
          case ForComprehensionMergeStrategy.OverwriteComprehensionFully =>
            Option.when(t1.structure != t2.structure)(Patch.replaceTree(t1, t2.syntax)).toList
          case ForComprehensionMergeStrategy.PreserveUserExpressions =>
            val merged = mergeForYieldTerm(t1, t2)
            Option.when(merged.structure != t1.structure)(Patch.replaceTree(t1, merged.syntax)).toList
        }
      case _ =>
        List.empty
    }

  private def mergeForTerm(original: Term.For, generated: Term.For): Term.For = {
    val mergedEnums = mergeEnumerators(original.enums, generated.enums)
    val mergedBody = mergeForBodyTerm(original.body, generated.body)
    original.copy(enums = mergedEnums, body = mergedBody)
  }

  private def mergeForYieldTerm(original: Term.ForYield, generated: Term.ForYield): Term.ForYield = {
    val mergedEnums = mergeEnumerators(original.enums, generated.enums)
    val mergedBody = mergeForBodyTerm(original.body, generated.body)
    original.copy(enums = mergedEnums, body = mergedBody)
  }

  private def mergeForBodyTerm(original: Term, generated: Term): Term =
    (original, generated) match {
      case (o: Term.For, g: Term.For) =>
        mergeForTerm(o, g)
      case (o: Term.ForYield, g: Term.ForYield) =>
        mergeForYieldTerm(o, g)
      case _ =>
        // preserve existing user expression by default
        original
    }

  private def mergeEnumerators(originalEnums: List[Enumerator], generatedEnums: List[Enumerator]): List[Enumerator] = {
    val generatedByKey = toUniqueMap(generatedEnums, enumeratorMergeKey, "for-comprehension qualifiers")
    var usedGeneratedKeys: Set[String] = Set.empty
    val mergedExisting = originalEnums.map { enum =>
      val key = enumeratorMergeKey(enum)
      generatedByKey.get(key) match {
        case Some(generatedEnum) if !usedGeneratedKeys.contains(key) =>
          usedGeneratedKeys += key
          generatedEnum
        case _ =>
          enum
      }
    }
    val newGenerated = generatedEnums.filter { enum =>
      val key = enumeratorMergeKey(enum)
      generatedByKey.contains(key) && !usedGeneratedKeys.contains(key)
    }
    mergedExisting ++ newGenerated
  }

  private def enumeratorMergeKey(enum: Enumerator): String = enum match {
    case Enumerator.Generator(pat, rhs) =>
      s"gen:${pat.structure}:${termShape(rhs)}"
    case Enumerator.Val(pat, rhs) =>
      s"val:${pat.structure}:${termShape(rhs)}"
    case Enumerator.Guard(cond) =>
      s"guard:${termShape(cond)}"
    case other =>
      s"${other.productPrefix}:${other.structure}"
  }

  private def termShape(term: Term): String = term match {
    case Term.Apply(fun, _) =>
      s"apply(${termShape(fun)})"
    case Term.ApplyType(fun, _) =>
      s"applyType(${termShape(fun)})"
    case Term.Select(qual, name) =>
      s"select(${termShape(qual)}.${name.value})"
    case Term.Name(name) =>
      s"name($name)"
    case Term.This(qual) =>
      s"this(${qual.value})"
    case Term.Super(thisp, superp) =>
      s"super(${thisp.value}.${superp.value})"
    case Term.Tuple(values) =>
      s"tupleArity=${values.size}"
    case Term.Block(stats) =>
      s"blockArity=${stats.size}"
    case other =>
      other.productPrefix
  }

  private def defSignatureKey(d: Defn.Def): String =
    d.body.tokens.headOption match {
      case Some(bodyFirstToken) =>
        d.tokens
          .takeWhile(_.pos.start < bodyFirstToken.pos.start)
          .map(_.text)
          .mkString("")
      case None =>
        d.name.value
    }

  private def toUniqueMap[T](values: List[T], key: T => String, what: String): Map[String, T] = {
    val grouped = values.groupBy(key)
    val ambiguousKeys = grouped.collect { case (k, v) if v.size > 1 => k }.toList.sorted
    if (ambiguousKeys.nonEmpty) {
      throw new IllegalArgumentException(
        s"[regenesca] Ambiguous merge keys for $what: ${ambiguousKeys.mkString(", ")}"
      )
    }
    grouped.collect { case (k, v) if v.size == 1 => k -> v.head }.toMap
  }

  private def patchCases(
      origSourceLastToken: Token,
      hasBody: Boolean,
      originalCases: List[Case],
      overwritingCases: List[Case]
  ): List[Patch] = {
    var usedOverwritingCases: Set[Case] = Set.empty
    val overwritingCasesMap = overwritingCases.map { c2 =>
      c2.pat.structure -> c2
    }.toMap
    val overwritePatches: List[Patch] = originalCases.flatMap { c1 =>
      overwritingCasesMap.get(c1.pat.structure) match {
        case Some(c2) =>
          usedOverwritingCases += c2
          patchTerms(origSourceLastToken, hasBody, c1.body, c2.body)
        case None =>
          List.empty
      }
    }
    val newCases = overwritingCases.filterNot(usedOverwritingCases)
    overwritePatches ++ newCases.map { c =>
      val indentedCase = StringUtils.indent(c.syntax, originalCases.last.pos.startColumn)
      Patch.addRight(originalCases.last, "\n" + indentedCase)
    }
  }
}

object SourceMerger {
  def apply(
      mergeDefBodies: Boolean = true,
      forComprehensionMergeStrategy: ForComprehensionMergeStrategy =
        ForComprehensionMergeStrategy.PreserveUserExpressions
  )(implicit dialect: Dialect): SourceMerger =
    new SourceMerger(mergeDefBodies, forComprehensionMergeStrategy)
}
