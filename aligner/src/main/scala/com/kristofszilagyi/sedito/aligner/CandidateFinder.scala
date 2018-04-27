package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.WordIndexRange

final class CandidateFinder(words: Set[WordIndexRange], wordSize: Int) {
  private val tooSmalls = words.filter(_.toWord.length < wordSize)
  private val fixedFinder = new FixedCandidateFinder(words -- tooSmalls, wordSize / 3)

  def possibleMatches(s: String): Set[WordIndexRange] = {
    fixedFinder.possibleMatches(s) ++ tooSmalls
  }
}
