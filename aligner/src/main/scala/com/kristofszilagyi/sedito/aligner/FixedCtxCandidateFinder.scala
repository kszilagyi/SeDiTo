package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.Aligner.WordWithContext
import com.kristofszilagyi.sedito.aligner.FixedCtxCandidateFinder._
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._

import scala.collection.mutable

object FixedCtxCandidateFinder {
  def allSubstrings(s: String, size: Int): Set[SubString] = {
    s.sliding(size).filter(_.length ==== size).map(SubString).toSet
  }
}

final case class SubString(s: String) extends AnyVal

final class FixedCtxCandidateFinder(contexts: Set[WordWithContext], substringSize: Int) {

  private def lookupTable(selector: WordWithContext => String): mutable.Map[SubString, Set[Aligner.WordWithContext]] = {
    val hashMap = new mutable.OpenHashMap[SubString, Set[Aligner.WordWithContext]]()
    contexts.foreach{ ctx =>
      val subs = allSubstrings(selector(ctx), substringSize)
      subs.foreach { sub =>
        val current = hashMap.getOrElse(sub, Set.empty)
        hashMap.put(sub, current + ctx)
      }
    }
    hashMap
  }

  private val beforeLookup = lookupTable(_.beforeContext)
  private val afterLookup = lookupTable(_.afterContext)

  private def search(s: String, lookupTable: mutable.Map[SubString, Set[Aligner.WordWithContext]]) = {
    val subs = allSubstrings(s, substringSize)
    subs.flatMap { sub =>
      lookupTable.getOrElse(sub, Set.empty)
    }
  }

  def possibleMatches(s: WordWithContext): Set[WordWithContext] ={
    search(s.beforeContext, beforeLookup) ++ search(s.afterContext, afterLookup)
  }
}
