package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.Aligner.WordWithContext
import com.kristofszilagyi.sedito.aligner.FixedCtxCandidateFinder._
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._

object FixedCtxCandidateFinder {
  def allSubstrings(s: String, size: Int): Set[SubString] = {
    s.sliding(size).filter(_.length ==== size).map(SubString).toSet
  }
}

final case class SubString(s: String) extends AnyVal

final class FixedCtxCandidateFinder(contexts: Set[WordWithContext], substringSize: Int) {

  private def lookupTable(selector: WordWithContext => String): Map[SubString, Set[Aligner.WordWithContext]] = {
    val immutableMap = (contexts.flatMap { ctx =>
      val subs = allSubstrings(selector(ctx), substringSize)
      subs.map(_ -> ctx)
    }.groupBy(_._1).map{case (sub, all) => sub -> all.map(_._2)})
    immutableMap
  }

  private val beforeLookup = lookupTable(_.beforeContext)
  private val afterLookup = lookupTable(_.afterContext)

  private def search(s: String, lookupTable: Map[SubString, Set[Aligner.WordWithContext]]) = {
    val subs = allSubstrings(s, substringSize)
    subs.flatMap { sub =>
      lookupTable.getOrElse(sub, Set.empty)
    }
  }

  def possibleMatches(s: WordWithContext): Set[WordWithContext] ={
    search(s.beforeContext, beforeLookup) ++ search(s.afterContext, afterLookup)
  }
}
