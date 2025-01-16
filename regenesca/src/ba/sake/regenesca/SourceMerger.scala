package ba.sake.regenesca

import scala.meta._
import scala.meta.contrib._

class SourceMerger(mergeDefBodies: Boolean) {

  def merge(originalSource: Source, overwriteSource: Source): Source = {
    val overwrittenStats =      overwriteStats(originalSource.stats, overwriteSource.stats)
    originalSource.copy(stats = overwrittenStats)
  }

  private def overwriteStats(
      originalStats: List[Stat],
      generatedStats: List[Stat],
      appendNewDefinitions: Boolean = true
  ): List[Stat] = {
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
      d2.name.value -> d2
    }.toMap
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
    val overwrittenOriginalStats = originalStats.map {
      case p1: Pkg =>
        overwritingPkgsMap.get(p1.name.value) match {
          case Some(p2) =>
            usedOverwritingStats += p2
            val pkgStats = overwriteStats(p1.stats, p2.stats)
            p1.copy(stats = pkgStats)
          case None => p1
        }
      case i1: Import =>
        overwritingImports.find(_.isEqual(i1)) match {
          case Some(i2) =>
            usedOverwritingStats += i2
            i1
          case None => i1
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
            v2
          case None => v1
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
            v2
          case None => v1
        }
      case d1: Defn.Def =>
        overwritingDefsMap.get(d1.name.value) match {
          case Some(d2) =>
            usedOverwritingStats += d2
            if (mergeDefBodies) {
              val mergedBody = merge2Terms(d1.body, d2.body)
              d2.copy(body = mergedBody)
            } else {
              d2
            }
          case None => d1
        }
      case e1: Defn.Enum =>
        overwritingEnumsMap.get(e1.name.value) match {
          case Some(e2) =>
            usedOverwritingStats += e2
            e2
          case None => e1
        }
      case c1: Defn.Class =>
        overwritingClassesMap.get(c1.name.value) match {
          case Some(c2) =>
            usedOverwritingStats += c2
            var usedOverwritingParamClauses: Set[Term.ParamClause] = Set.empty
            val overwrittenParamClauses =
              c1.ctor.paramClauses.zipWithIndex.map { case (paramClause1, i) =>
                c2.ctor.paramClauses.lift(i) match {
                  case Some(paramClause2) =>
                    usedOverwritingParamClauses += paramClause2
                    val overwritingParamsMap = paramClause2.values
                      .map(p2 => p2.name.value -> p2)
                      .toMap
                    var usedOverwritingParams: Set[Term.Param] = Set.empty
                    val overwrittenParams = paramClause1.values.map { param1 =>
                      overwritingParamsMap.get(param1.name.value) match {
                        case Some(overwritingParam) =>
                          usedOverwritingParams += overwritingParam
                          overwritingParam
                        case None =>
                          param1
                      }
                    }
                    val params = overwrittenParams ++ paramClause2.values.filterNot(usedOverwritingParams)
                    paramClause1.copy(values = params)
                  case None => paramClause1
                }
              }
            val paramClauses = overwrittenParamClauses ++ c2.ctor.paramClauses.filterNot(usedOverwritingParamClauses)
            val mergedTemplStats = overwriteStats(c1.templ.stats, c2.templ.stats)
            val mergedTempl = c1.templ.copy(stats = mergedTemplStats)
            val mods = (c1.ctor.mods ++ c2.ctor.mods).distinct
            c1.copy(
              templ = mergedTempl,
              ctor = c1.ctor.copy(paramClauses = paramClauses, mods = mods, name = c1.ctor.name)
            )
          case None => c1
        }
      case t1: Defn.Trait =>
        overwritingTraitsMap.get(t1.name.value) match {
          case Some(t2) =>
            usedOverwritingStats += t2
            val mergedTemplStats = overwriteStats(t1.templ.stats, t2.templ.stats)
            val mergedTempl = t1.templ.copy(stats = mergedTemplStats)
            t1.copy(templ = mergedTempl)
          case None => t1
        }
      case o1: Defn.Object =>
        overwritingObjectsMap.get(o1.name.value) match {
          case Some(o2) =>
            usedOverwritingStats += o2
            val mergedTemplStats =
              overwriteStats(o1.templ.stats, o2.templ.stats)
            val mergedTempl = o1.templ.copy(stats = mergedTemplStats)
            o1.copy(templ = mergedTempl)
          case None => o1
        }
      case t1: Defn.Type =>
        overwritingTypesMap.get(t1.name.value) match {
          case Some(t2) =>
            usedOverwritingStats += t2
            t2
          case None => t1
        }
      case g1: Defn.Given =>
        // try to encode a "name" for anonymous givens...
        val key = g1.name + g1.templ.inits.map(_.structure).mkString("-")
        overwritingGivensMap.get(key) match {
          case Some(g2) =>
            usedOverwritingStats += g2
            g2
          case None => g1
        }
      case g1: Defn.GivenAlias =>
        overwritingGivenAliasesMap.get(g1.decltpe.structure) match {
          case Some(g2) =>
            usedOverwritingStats += g2
            g2
          case None =>
            g1
        }
      // leave other statements intact
      case other => other
    }.toBuffer
    /* insert new stats at appropriate position */
    val newStats = overwritingStats.filterNot(usedOverwritingStats)
    locally {
      val newImports = newStats.collect { case i2: Import => i2 }
      usedOverwritingStats ++= newImports
      val indexOfLastImport = overwrittenOriginalStats.lastIndexWhere(s => s.isInstanceOf[Import])
      overwrittenOriginalStats.insertAll(indexOfLastImport + 1, newImports)
    }
    locally {
      val newVals = newStats.collect { case v2: Defn.Val => v2 }
      usedOverwritingStats ++= newVals
      val indexOfLastValVar =
        overwrittenOriginalStats.lastIndexWhere(s => s.isInstanceOf[Defn.Val] || s.isInstanceOf[Defn.Var])
      overwrittenOriginalStats.insertAll(indexOfLastValVar + 1, newVals)
    }
    locally {
      val newVars = newStats.collect { case v2: Defn.Var => v2 }
      usedOverwritingStats ++= newVars
      val indexOfLastValVar =
        overwrittenOriginalStats.lastIndexWhere(s => s.isInstanceOf[Defn.Val] || s.isInstanceOf[Defn.Var])
      overwrittenOriginalStats.insertAll(indexOfLastValVar + 1, newVars)
    }
    locally {
      val otherStats = overwritingStats.filterNot(usedOverwritingStats)
      val indexOfLastImport = overwrittenOriginalStats.lastIndexWhere(s => s.isInstanceOf[Import])
      if (appendNewDefinitions) overwrittenOriginalStats.appendAll(otherStats)
      else overwrittenOriginalStats.insertAll(indexOfLastImport + 1, otherStats) // in a block
    }
    overwrittenOriginalStats.toList
  }

  private def merge2Terms(originalTerm: Term, overwriteTerm: Term): Term =
    (originalTerm, overwriteTerm) match {
      case (t1: Term.Block, t2: Term.Block) =>
        val mergedStats = overwriteStats(t1.stats, t2.stats, appendNewDefinitions = false)
        t1.copy(stats = mergedStats)
      case (t1: Term.Apply, t2: Term.Apply) =>
        if ( // only handling one-arg functions...
          t1.args.length == 1 && t2.args.length == 1 &&
          t1.fun.isInstanceOf[Term.Name] &&
          t2.fun.isInstanceOf[Term.Name] &&
          t1.fun.asInstanceOf[Term.Name].value ==
            t2.fun.asInstanceOf[Term.Name].value
        ) {
          val mergedArgClause =
            t1.argClause.copy(values = List(merge2Terms(t1.argClause.values.head, t2.argClause.values.head)))
          t1.copy(fun = t1.fun, argClause = mergedArgClause)
        } else {
          originalTerm
        }
      case (t1: Term.Apply, t2: Term.Block) =>
        // if it's just an expression like Response.withBody("")
        // and we add a block
        // just treat that expr as a block and merge them

        merge2Terms(q"{ ..${List(t1)} }", t2)
      // val stats = t2.stats ++ List(t1)
      // t2.copy(stats = stats)
      case (t1: Term.PartialFunction, t2: Term.PartialFunction) =>
        val mergedCases = mergeCases(t1.cases, t2.cases)
        t1.copy(cases = mergedCases)
      case _ => originalTerm
    }

  private def mergeCases(
      originalCases: List[Case],
      overwritingCases: List[Case]
  ): List[Case] = {
    var usedOverwritingCases: Set[Case] = Set.empty
    val overwritingCasesMap = overwritingCases.map { c2 =>
      c2.pat.structure -> c2
    }.toMap
    val overwrittenOriginalCases = originalCases.map { c1 =>
      overwritingCasesMap.get(c1.pat.structure) match {
        case Some(c2) =>
          usedOverwritingCases += c2
          val mergedBody = merge2Terms(c1.body, c2.body)
          c1.copy(body = mergedBody)
        case None => c1
      }
    }
    val newCases = overwritingCases.filterNot(usedOverwritingCases)
    overwrittenOriginalCases ++ newCases
  }
}

object SourceMerger {
  def apply(mergeDefBodies: Boolean = true): SourceMerger =
    new SourceMerger(mergeDefBodies)
}
