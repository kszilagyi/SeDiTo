package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import FixedCandidateFinder._
import com.kristofszilagyi.sedito.common.WordIndexRange

object FixedCandidateFinder {
  def allSubstrings(s: String, size: Int): Set[SubString] = {
    s.sliding(size).filter(_.length ==== size).map(SubString).toSet
  }
}

final case class SubString(s: String) extends AnyVal

final class FixedCandidateFinder(words: Set[WordIndexRange], substringSize: Int) {

  private val lookupTable = {
    words.flatMap { word =>
      val subs = allSubstrings(word.toWord, substringSize)
      subs.map(_ -> word)
    }.groupBy(_._1).map{case (sub, all) => sub -> all.map(_._2)}
  }

  def possibleMatches(s: String): Set[WordIndexRange] ={
    val subs = allSubstrings(s, substringSize)
    subs.flatMap { sub =>
      lookupTable.getOrElse(sub, Set.empty)
    }
  }
}
