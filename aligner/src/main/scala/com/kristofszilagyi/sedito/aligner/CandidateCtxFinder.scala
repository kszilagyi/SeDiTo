package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.Aligner.WordWithContext

final class CandidateCtxFinder(contexts: Set[WordWithContext], contextSize: Int) {
  private val tooSmalls = contexts.filter(ctx => ctx.beforeContext.length < contextSize || ctx.afterContext.length < contextSize)
  private val fixedFinder = new FixedCtxCandidateFinder(contexts -- tooSmalls, contextSize / 3)
  //todo mapValuesNow
  private val positionAgnosticWordWithContexts = contexts.map(c => c.positionAgnostic -> c).groupBy(_._1).map {
    case (k, v) => k -> v.map(_._2)
  }

  def possibleMatches(s: WordWithContext): Set[WordWithContext] = {
    val exactMatches = positionAgnosticWordWithContexts.getOrElse(s.positionAgnostic, Set.empty)
    if(exactMatches.nonEmpty) exactMatches
    else fixedFinder.possibleMatches(s) ++ tooSmalls
  }
}
