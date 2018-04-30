package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.Aligner.WordWithContext

final class CandidateCtxFinder(contexts: Set[WordWithContext]) {
  //todo mapValuesNow
  private val positionAgnosticWordWithContexts = contexts.map(c => c.positionAgnostic -> c).groupBy(_._1).map {
    case (k, v) => k -> v.map(_._2)
  }

  def possibleMatches(s: WordWithContext): Set[WordWithContext] = {
    val exactMatches = positionAgnosticWordWithContexts.getOrElse(s.positionAgnostic, Set.empty)
    if(exactMatches.nonEmpty) exactMatches
    else contexts
  }
}
