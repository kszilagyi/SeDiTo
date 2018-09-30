package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.AssertionEx._
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.common._

import scala.collection.Searching._

object TrivialContextCorrector {
  @SuppressWarnings(Array(Warts.Var, Warts.While))
  private def localCorrect(startLeft: Int, startRight: Int, leftWords: IndexedSeq[Selection], rightWords: IndexedSeq[Selection]) = {
    var l = startLeft
    var r = startRight
    val builder = IndexedSeq.newBuilder[WordMatch]
    var done = false
    while(l < leftWords.size && r < rightWords.size && !done) {
      val left = leftWords(l)
      val right = rightWords(r)
      if (left.toText ==== right.toText) {
        discard(builder += WordMatch(left, right))
      } else {
        done = true
      }
      l += 1; r += 1
    }
    builder.result()
  }
  def correct(left: FullText, right: FullText, alignment: UnambiguousWordAlignment): UnambiguousWordAlignment = {
    val sortedMatches = alignment.matches.toIndexedSeq.sortBy(_.left)
    val leftWords = Wordizer.toWordIndices(left.s)
    val rightWords = Wordizer.toWordIndices(right.s)
    val atStartRes = localCorrect(0, 0, leftWords, rightWords)
    val middleRes = sortedMatches.flatMap { m =>
      val maybeLeftIdx = leftWords.search(m.left)(Ordering.by(_.absoluteFrom))
      val maybeRightIdx = rightWords.search(m.right)(Ordering.by(_.absoluteFrom))
      (maybeLeftIdx, maybeRightIdx) match {
        case (Found(leftIdx), Found(rightIdx)) =>
          localCorrect(leftIdx, rightIdx, leftWords, rightWords)
        case other => fail(s"match $m is not found in texts: $other")
      }
    }
    UnambiguousWordAlignment(alignment.matches ++ atStartRes ++ middleRes)
  }
}
