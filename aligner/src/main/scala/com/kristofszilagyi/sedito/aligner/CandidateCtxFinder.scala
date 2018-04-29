package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.Aligner.WordWithContext

final class CandidateCtxFinder(contexts: Set[WordWithContext], contextSize: Int) {
  private val tooSmalls = contexts.filter(ctx => ctx.beforeContext.length < contextSize || ctx.afterContext.length < contextSize)
  private val fixedFinder = new FixedCtxCandidateFinder(contexts -- tooSmalls, contextSize / 3)

  def possibleMatches(s: WordWithContext): Set[WordWithContext] = {
    fixedFinder.possibleMatches(s) ++ tooSmalls
  }
}
