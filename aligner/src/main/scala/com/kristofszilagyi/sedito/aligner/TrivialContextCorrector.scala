package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common._
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts.discard

object TrivialContextCorrector {
  @SuppressWarnings(Array(Warts.Var, Warts.While))
  def correct(left: FullText, right: FullText, alignment: UnambiguousWordAlignment): UnambiguousWordAlignment = {
    //val sortedMatches = alignment.matches.toIndexedSeq.sortBy(_.left)
    val leftWords = Wordizer.toWordIndices(left.s)
    val rightWords = Wordizer.toWordIndices(right.s)
    var i = 0
    var j = 0
    val builder = IndexedSeq.newBuilder[WordMatch]
    var done = false
    while(i < leftWords.size && j < rightWords.size && !done) {
      val left = leftWords(i)
      val right = rightWords(j)
      if (left.toText ==== right.toText) {
        discard(builder += WordMatch(left, right))
      } else {
        done = true
      }
      i += 1; j += 1
    }
    UnambiguousWordAlignment(alignment.matches ++ builder.result())
  }
}
