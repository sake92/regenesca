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

  private val MaxDiagnosticSyntaxLength = 120

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
    val overwritingValsMap = toUniqueKeyedMap(
      overwritingStats.collect { case v2: Defn.Val => v2 },
      v => valVarPatKeys(v.pats),
      "val names/patterns"
    )
    val overwritingVarsMap = toUniqueKeyedMap(
      overwritingStats.collect { case v2: Defn.Var => v2 },
      v => valVarPatKeys(v.pats),
      "var names/patterns"
    )
    val overwritingDefsMap = overwritingStats.collect { case d2: Defn.Def =>
      d2
    }
    val overwritingDefsBySignatureMap = toUniqueMap(overwritingDefsMap, defSignatureKey, "def signatures")
    val overwritingEnumsMap = toUniqueMap(
      overwritingStats.collect { case e2: Defn.Enum => e2 },
      _.name.value,
      "enum names"
    )
    val overwritingClassesMap = toUniqueMap(
      overwritingStats.collect { case c2: Defn.Class => c2 },
      _.name.value,
      "class names"
    )
    val overwritingTraitsMap = toUniqueMap(
      overwritingStats.collect { case t2: Defn.Trait => t2 },
      _.name.value,
      "trait names"
    )
    val overwritingObjectsMap = toUniqueMap(
      overwritingStats.collect { case o2: Defn.Object => o2 },
      _.name.value,
      "object names"
    )
    val overwritingTypesMap = toUniqueMap(
      overwritingStats.collect { case t2: Defn.Type => t2 },
      _.name.value,
      "type names"
    )
    val overwritingGivensMap = toUniqueMap(
      overwritingStats.collect { case g2: Defn.Given => g2 },
      givenMergeKey,
      "given definitions"
    )
    val overwritingGivenAliasesMap = toUniqueMap(
      overwritingStats.collect { case g2: Defn.GivenAlias => g2 },
      _.decltpe.structure,
      "given aliases"
    )

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
        findByAnyKey(overwritingValsMap, valVarPatKeys(v1.pats), "original val names/patterns") match {
          case Some(v2) =>
            usedOverwritingStats += v2
            Option.when(v1.structure != v2.structure)(Patch.replaceTree(v1, v2.syntax)).toList
          case None =>
            List.empty
        }
      case v1: Defn.Var =>
        findByAnyKey(overwritingVarsMap, valVarPatKeys(v1.pats), "original var names/patterns") match {
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
        overwritingGivensMap.get(givenMergeKey(g1)) match {
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
        mergeBlockTerms(origSourceLastToken, hasBody, t1, t2)
      case (t1: Term.Apply, t2: Term.Apply) =>
        // Recurse into single-arg function applications when fun structures match
        // (handles both simple names like Response.withBody(...) and complex funs like HttpRoutes.of[IO](...))
        if (t1.args.length == 1 && t2.args.length == 1 && t1.fun.structure == t2.fun.structure) {
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

  /* Merge block contents: dispatch structural terms (ForYield, For, PartialFunction, Block) to
   * patchTerms, and def-level stats (Val, Def, etc.) to patchStats.
   * Terms are matched positionally; new generated structural terms are appended. */
  private def mergeBlockTerms(
      origSourceLastToken: Token,
      hasBody: Boolean,
      originalBlock: Term.Block,
      generatedBlock: Term.Block
  ): List[Patch] = {
    // Terms that have meaningful merge semantics (vs. leaf expressions like Term.Apply)
    def isStructuralTerm(s: Stat): Boolean = s match {
      case _: Term.For | _: Term.ForYield | _: Term.PartialFunction | _: Term.Block => true
      case _ => false
    }
    val (origTerms, origDefs) = originalBlock.stats.partition(isStructuralTerm)
    val (genTerms, genDefs) = generatedBlock.stats.partition(isStructuralTerm)

    val origTermTrees = origTerms.collect { case t: Term => t }
    val genTermTrees = genTerms.collect { case t: Term => t }
    val generatedTermByKey = toUniqueMap(genTermTrees, structuralTermMergeKey, "structural block terms")

    // Key-based matching for structural terms
    val termPatches = List.newBuilder[Patch]
    val matchedGenTermKeys = scala.collection.mutable.Set.empty[String]
    origTermTrees.foreach { origTerm =>
      val key = structuralTermMergeKey(origTerm)
      generatedTermByKey.get(key) match {
        case Some(genTerm) =>
          matchedGenTermKeys += key
          termPatches ++= patchTerms(origSourceLastToken, hasBody, origTerm, genTerm)
        case None =>
        // leave unchanged
      }
    }

    // Append new unmatched structural terms after last original term (or last original stat)
    val unmatchedGenTerms = genTermTrees.filterNot(t => matchedGenTermKeys.contains(structuralTermMergeKey(t)))
    if (unmatchedGenTerms.nonEmpty) {
      val insertionPoint = origTerms.lastOption.orElse(originalBlock.stats.lastOption)
      insertionPoint.foreach { ref =>
        unmatchedGenTerms.foreach { term =>
          val indented = StringUtils.indent(term.syntax, ref.pos.startColumn)
          termPatches += Patch.addRight(ref, "\n" + indented)
        }
      }
    }

    // Def-level stats (Val, Def, Import, Class, etc.) handled via patchStats
    val defPatches = patchStats(origSourceLastToken, hasBody, origDefs, genDefs, appendNewDefinitions = false)

    termPatches.result() ++ defPatches
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
      case (o: Term.Block, g: Term.Block) =>
        // Unwrap single-statement blocks containing for-comprehensions (e.g. yield { for { ... } yield expr })
        (o.stats.headOption, g.stats.headOption) match {
          case (Some(os: Term), Some(gs: Term)) if o.stats.size == 1 && g.stats.size == 1 =>
            val merged = mergeForBodyTerm(os, gs)
            Term.Block(List(merged))
          case _ =>
            // Multi-statement blocks: preserve user expression
            original
        }
      case _ =>
        // preserve existing user expression by default
        original
    }

  private def mergeEnumerators(originalEnums: List[Enumerator], generatedEnums: List[Enumerator]): List[Enumerator] = {
    val generatedByKey = toUniqueMap(generatedEnums, enumeratorMergeKey, "for-comprehension qualifiers")
    val mergedExisting = originalEnums.map { enum =>
      val key = enumeratorMergeKey(enum)
      generatedByKey.getOrElse(key, enum)
    }
    val replacedKeys = originalEnums.map(enumeratorMergeKey).toSet.intersect(generatedByKey.keySet)
    val newGenerated = generatedEnums.filter { enum =>
      val key = enumeratorMergeKey(enum)
      !replacedKeys.contains(key)
    }
    mergedExisting ++ newGenerated
  }

  private def enumeratorMergeKey(enum: Enumerator): String = enum match {
    // Prefixes separate enumerator kinds to avoid key collisions across types.
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
    case f: Term.For =>
      s"for(${f.enums.map(enumeratorMergeKey).mkString("|")})"
    case f: Term.ForYield =>
      s"forYield(${f.enums.map(enumeratorMergeKey).mkString("|")})"
    case pf: Term.PartialFunction =>
      s"partialFunction(${pf.cases.map(caseMergeKey).mkString("|")})"
    case other =>
      other.productPrefix
  }

  private def structuralTermMergeKey(term: Term): String = term match {
    case _: Term.For | _: Term.ForYield | _: Term.PartialFunction =>
      termShape(term)
    case b: Term.Block =>
      val head = b.stats.collectFirst { case t: Term => termShape(t) }.getOrElse("empty")
      s"block:${b.stats.size}:$head"
    case other =>
      s"${other.productPrefix}:${termShape(other)}"
  }

  /** Extracts method signature tokens (before body) to distinguish overloads by full signature. */
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

  private def toUniqueMap[T <: Tree](
      values: List[T],
      key: T => String,
      elementDescription: String
  ): Map[String, T] = {
    val grouped = values.groupBy(key)
    val ambiguousKeys = grouped.collect { case (k, v) if v.size > 1 => k }.toList.sorted
    if (ambiguousKeys.nonEmpty) {
      val details = ambiguousKeys.map { k =>
        val examples =
          grouped(k).take(2).map(_.syntax.take(MaxDiagnosticSyntaxLength)).mkString(" | ")
        s"$k => $examples"
      }
      throw new IllegalArgumentException(
        s"[regenesca] Ambiguous merge keys for $elementDescription: ${details.mkString("; ")}"
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
    val overwritingCasesMap = toUniqueMap(overwritingCases, caseMergeKey, "case branches")
    val overwritePatches: List[Patch] = originalCases.flatMap { c1 =>
      overwritingCasesMap.get(caseMergeKey(c1)) match {
        case Some(c2) =>
          usedOverwritingCases += c2
          patchTerms(origSourceLastToken, hasBody, c1.body, c2.body)
        case None =>
          List.empty
      }

      private def caseMergeKey(c: Case): String =
        s"${c.pat.structure}|guard=${c.cond.map(termShape).getOrElse("_")}|body=${termShape(c.body)}"

      private def givenMergeKey(g: Defn.Given): String =
        s"${g.name}|${g.templ.inits.map(_.structure).mkString("-")}"

      private def valVarPatKeys(pats: List[Pat]): List[String] =
        pats.flatMap(patVarKeys).distinct

      private def patVarKeys(pat: Pat): List[String] = pat match {
        case Pat.Var(name) => List(name.value)
        case Pat.Bind(lhs, rhs) => patVarKeys(lhs) ++ patVarKeys(rhs)
        case Pat.Alternative(lhs, rhs) => patVarKeys(lhs) ++ patVarKeys(rhs)
        case Pat.Tuple(args) => args.flatMap(patVarKeys)
        case Pat.Extract(_, args) => args.flatMap(patVarKeys)
        case Pat.ExtractInfix(lhs, _, rhs) => patVarKeys(lhs) ++ rhs.flatMap(patVarKeys)
        case Pat.Interpolate(_, args) => args.flatMap(patVarKeys)
        case Pat.Typed(lhs, _) => patVarKeys(lhs)
        case Pat.Repeated(lhs) => patVarKeys(lhs)
        case _ => List.empty
      }

      private def toUniqueKeyedMap[T <: Tree](
          values: List[T],
          keys: T => List[String],
          elementDescription: String
      ): Map[String, T] = {
        val keyedValues = values.flatMap(v => keys(v).distinct.map(_ -> v))
        val grouped = keyedValues.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
        val ambiguousKeys =
          grouped.collect { case (k, vs) if vs.map(_.structure).distinct.size > 1 => k }.toList.sorted
        if (ambiguousKeys.nonEmpty) {
          val details = ambiguousKeys.map { k =>
            val examples = grouped(k).distinct.take(2).map(_.syntax.take(MaxDiagnosticSyntaxLength)).mkString(" | ")
            s"$k => $examples"
          }
          throw new IllegalArgumentException(
            s"[regenesca] Ambiguous merge keys for $elementDescription: ${details.mkString("; ")}"
          )
        }
        grouped.collect { case (k, vs) if vs.nonEmpty => k -> vs.head }.toMap
      }

      private def findByAnyKey[T](
          valuesByKey: Map[String, T],
          keys: List[String],
          keyDescription: String
      ): Option[T] = {
        val matches = keys.flatMap(valuesByKey.get).distinct
        if (matches.size > 1) {
          throw new IllegalArgumentException(
            s"[regenesca] Ambiguous merge key lookup for $keyDescription: ${keys.mkString(", ")}"
          )
        }
        matches.headOption
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
