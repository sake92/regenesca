package ba.sake.regenesca

import scala.collection.decorators._
import scala.meta._
import scala.meta.contrib._

class SourceMerger(mergeDefBody: Boolean) {

  def merge(original: Source, overwrite: Source): Source = {
    val overwittenStats = mergeStats(original.stats, overwrite.stats)
    original.copy(stats = overwittenStats)
  }

  // merges packages, classes, methods by name
  // overwriteStats + originalStats_that_are_not_overwritten
  private def mergeStats(
      originalStats: List[Stat],
      overwriteStats: List[Stat]
  ): List[Stat] = {
    val originalPackages = originalStats.collect { case p: Pkg => p }
    val overwritePackages = overwriteStats.collect { case p: Pkg => p }
    val mergedPackages = mergePackages(originalPackages, overwritePackages)

    val originalImports = originalStats.collect { case i: Import => i }
    val overwriteImports = overwriteStats.collect { case i: Import => i }
    val mergedImports = mergeImports(originalImports, overwriteImports)

    // this kinda works for top-level stats, not so sure about defs etc
    val originalValDefs = originalStats.collect { case v: Defn.Val => v }
    val overwriteValDefs = overwriteStats.collect { case v: Defn.Val => v }
    val mergedValDefs = mergeValDefs(originalValDefs, overwriteValDefs)

    val originalVarDefs = originalStats.collect { case v: Defn.Var => v }
    val overwriteVarDefs = overwriteStats.collect { case v: Defn.Var => v }
    val mergedVarDefs = mergeVarDefs(originalVarDefs, overwriteVarDefs)

    val originalMethodDefs = originalStats.collect { case d: Defn.Def => d }
    val overwriteMethodDefs = overwriteStats.collect { case d: Defn.Def => d }
    val mergedMethodDefs =
      mergeMethodDefs(originalMethodDefs, overwriteMethodDefs)

    val originalEnums = originalStats.collect { case e: Defn.Enum => e }
    val overwriteEnums = overwriteStats.collect { case e: Defn.Enum => e }
    val mergedEnums = mergeEnums(originalEnums, overwriteEnums)

    val originalClasses = originalStats.collect { case c: Defn.Class => c }
    val overwriteClasses = overwriteStats.collect { case c: Defn.Class => c }
    val mergedClasses = mergeClasses(originalClasses, overwriteClasses)

    val originalObjects = originalStats.collect { case o: Defn.Object => o }
    val overwriteObjects = overwriteStats.collect { case o: Defn.Object => o }
    val mergedObjects = mergeObjects(originalObjects, overwriteObjects)

    val originalTypes = originalStats.collect { case t: Defn.Type => t }
    val overwriteTypes = overwriteStats.collect { case t: Defn.Type => t }
    val mergedTypes = mergeTypes(originalTypes, overwriteTypes)

    val originalTerms = originalStats.collect { case t: Term => t }
    val overwriteTerms = overwriteStats.collect { case t: Term => t }
    // val mergedTerms = mergeTerms(originalTerms, overwriteTerms)
    val terms = originalStats.collect { case t: Term => t }

    mergedPackages ++ mergedImports ++
      mergedValDefs ++ mergedVarDefs ++
      mergedMethodDefs ++
      mergedEnums ++ mergedClasses ++
      mergedObjects ++
      mergedTypes ++
      terms
  }

  private def mergePackages(
      originalPackages: List[Pkg],
      overwritePackages: List[Pkg]
  ): List[Pkg] = {
    val originalPackagesMap =
      originalPackages.map(p => p.name.value -> p).toOrderedMap
    val overwritePackagesMap =
      overwritePackages.map(p => p.name.value -> p).toOrderedMap
    val joined = originalPackagesMap.fullOuterJoin(overwritePackagesMap)
    val mergedPackages = joined.values.collect {
      case (Some(p1), Some(p2)) =>
        val mergedStats = mergeStats(p1.stats, p2.stats)
        p2.copy(stats = mergedStats)
      case (Some(p1), None) => p1
      case (None, Some(p2)) => p2
    }
    mergedPackages.toList
  }

  private def mergeImports(
      originalImports: List[Import],
      overwriteImports: List[Import]
  ): List[Import] = {
    val newImports = overwriteImports.filterNot { i =>
      originalImports.exists(_.isEqual(i))
    }
    (originalImports ++ newImports)
  }

  private def mergeEnums(
      originalEnums: List[Defn.Enum],
      overwriteEnums: List[Defn.Enum]
  ): List[Defn.Enum] = {
    val originalEnumsMap =
      originalEnums.map(c => c.name.value -> c).toOrderedMap
    val overwriteEnumsMap =
      overwriteEnums.map(c => c.name.value -> c).toOrderedMap
    val joined = originalEnumsMap.fullOuterJoin(overwriteEnumsMap)
    val mergedEnums = joined.values.collect {
      case (Some(_), Some(e2)) => e2
      case (Some(e1), None)    => e1
      case (None, Some(e2))    => e2
    }
    mergedEnums.toList
  }

  private def mergeClasses(
      originalClasses: List[Defn.Class],
      overwriteClasses: List[Defn.Class]
  ): List[Defn.Class] = {
    val originalClassesMap =
      originalClasses.map(c => c.name.value -> c).toOrderedMap
    val overwriteClassesMap =
      overwriteClasses.map(c => c.name.value -> c).toOrderedMap
    val joined = originalClassesMap.fullOuterJoin(overwriteClassesMap)
    val mergedClasses = joined.values.collect {
      case (Some(c1), Some(c2)) =>
        val mergedTemplStats = mergeStats(c1.templ.stats, c2.templ.stats)
        val mergedTempl = c2.templ.copy(stats = mergedTemplStats)
        c2.copy(templ = mergedTempl)
      case (Some(c1), None) => c1
      case (None, Some(c2)) => c2
    }
    mergedClasses.toList
  }

  private def mergeObjects(
      originalObjects: List[Defn.Object],
      overwriteObjects: List[Defn.Object]
  ): List[Defn.Object] = {
    val originalObjectsMap =
      originalObjects.map(o => o.name.value -> o).toOrderedMap
    val overwriteObjectsMap =
      overwriteObjects.map(o => o.name.value -> o).toOrderedMap
    val joined = originalObjectsMap.fullOuterJoin(overwriteObjectsMap)
    val mergedObjects = joined.values.collect {
      case (Some(o1), Some(o2)) =>
        val mergedTemplStats = mergeStats(o1.templ.stats, o2.templ.stats)
        val mergedTempl = o2.templ.copy(stats = mergedTemplStats)
        o2.copy(templ = mergedTempl)
      case (Some(o1), None) => o1
      case (None, Some(o2)) => o2
    }
    mergedObjects.toList
  }

  private def mergeTypes(
      originalTypes: List[Defn.Type],
      overwriteTypes: List[Defn.Type]
  ): List[Defn.Type] = {
    val originalTypesMap =
      originalTypes.map(t => t.name.value -> t).toOrderedMap
    val overwriteTypesMap =
      overwriteTypes.map(t => t.name.value -> t).toOrderedMap
    val joined = originalTypesMap.fullOuterJoin(overwriteTypesMap)
    val mergedTypes = joined.values.collect {
      case (Some(t1), Some(t2)) => t2
      case (Some(t1), None)     => t1
      case (None, Some(t2))     => t2
    }
    mergedTypes.toList
  }

  private def mergeValDefs(
      originalValDefs: List[Defn.Val],
      overwriteValDefs: List[Defn.Val]
  ): List[Defn.Val] = {
    val originalValDefsMap =
      originalValDefs.flatMap { v =>
        v.pats.headOption.collect { case p: Pat.Var =>
          p.name.value -> v
        }
      }.toOrderedMap
    val overwriteValDefsMap =
      overwriteValDefs.flatMap { v =>
        v.pats.headOption.collect { case p: Pat.Var =>
          p.name.value -> v
        }
      }.toOrderedMap
    val joined = originalValDefsMap.fullOuterJoin(overwriteValDefsMap)
    val mergedVals = joined.values.collect {
      // v2 wins here, no questions asked. For now
      case (Some(_), Some(v2)) => v2
      case (Some(v1), None)    => v1
      case (None, Some(v2))    => v2
    }
    mergedVals.toList
  }

  private def mergeVarDefs(
      originalVarDefs: List[Defn.Var],
      overwriteVarDefs: List[Defn.Var]
  ): List[Defn.Var] = {
    val originalVarDefsMap =
      originalVarDefs.flatMap { v =>
        v.pats.headOption.collect { case p: Pat.Var =>
          p.name.value -> v
        }
      }.toOrderedMap
    val overwriteVarDefsMap =
      overwriteVarDefs.flatMap { v =>
        v.pats.headOption.collect { case p: Pat.Var =>
          p.name.value -> v
        }
      }.toOrderedMap
    val joined = originalVarDefsMap.fullOuterJoin(overwriteVarDefsMap)
    val mergedVals = joined.values.collect {
      // v2 wins here, no questions asked. For now
      case (Some(_), Some(v2)) => v2
      case (Some(v1), None)    => v1
      case (None, Some(v2))    => v2
    }
    mergedVals.toList
  }

  private def mergeMethodDefs(
      originalMethodDefs: List[Defn.Def],
      overwriteMethodDefs: List[Defn.Def]
  ): List[Defn.Def] = {
    val originalMethodDefsMap =
      originalMethodDefs.map(d => d.name.value -> d).toOrderedMap
    val overwriteMethodDefsMap =
      overwriteMethodDefs.map(d => d.name.value -> d).toOrderedMap
    val joined = originalMethodDefsMap.fullOuterJoin(overwriteMethodDefsMap)
    val mergedDefs = joined.values.collect {
      case (Some(d1), Some(d2)) =>
        if (mergeDefBody) {
          val mergedBody = merge2Terms(d1.body, d2.body)
          d2.copy(body = mergedBody)
        } else {
          d2 // dont bother, just overwrite as new
        }
      case (Some(d1), None) => d1
      case (None, Some(d2)) => d2
    }
    mergedDefs.toList
  }

  private def merge2Terms(originalTerm: Term, overwriteTerm: Term): Term =
    (originalTerm, overwriteTerm) match {
      case (t1: Term.Block, t2: Term.Block) =>
        // this will only merge val definitions, maybe overwrite them
        val mergedStats = mergeStats(t1.stats, t2.stats)
        t1.copy(stats = mergedStats)
      case (t1: Term.Apply, t2: Term.Apply) =>
        if ( // only handling one-arg functions...
          t1.args.length == 1 && t2.args.length == 1 &&
          t1.fun.asInstanceOf[Term.Name].value ==
            t2.fun.asInstanceOf[Term.Name].value
        ) {
          val mergedArgClause = t1.argClause.copy(values =
            List(
              merge2Terms(t1.argClause.values.head, t2.argClause.values.head)
            )
          )
          t1.copy(fun = t1.fun, argClause = mergedArgClause)
        } else {
          originalTerm
        }
      case (t1: Term.PartialFunction, t2: Term.PartialFunction) =>
        val mergedCases = mergeCases(t1.cases, t2.cases)
        t1.copy(cases = mergedCases)
      case _ => originalTerm
    }

    private def mergeCases(
        originalCases: List[Case],
        overwriteCases: List[Case]
    ): List[Case] = {
      val originalCasesMap =
        originalCases.map(c => c.pat.structure -> c).toOrderedMap
      val overwriteCasesMap =
        overwriteCases.map(c => c.pat.structure -> c).toOrderedMap
      val joined = originalCasesMap.fullOuterJoin(overwriteCasesMap)
      val mergedDefs = joined.values.collect {
        case (Some(c1), Some(c2)) =>
          val mergedBody = merge2Terms(c1.body, c2.body)
          c2.copy(body = mergedBody)
        case (Some(c1), None) => c1
        case (None, Some(c2)) => c2
      }
      mergedDefs.toList
    }

  implicit class Seq2MapOps[K, V](seq: Seq[(K, V)]) {
    def toOrderedMap: scala.collection.immutable.SeqMap[K, V] = {
      val ordMap = scala.collection.mutable.LinkedHashMap.empty[K, V]
      seq.foreach { case (k, v) =>
        ordMap(k) = v
      }
      ordMap.to(scala.collection.immutable.SeqMap)
    }
  }
}

object SourceMerger {
  def apply(mergeDefBody: Boolean = true): SourceMerger =
    new SourceMerger(mergeDefBody)
}
