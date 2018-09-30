package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.AssertionEx._
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.common._

import scala.collection.Searching._

object TrivialContextCorrector {
  @SuppressWarnings(Array(Warts.Var, Warts.While))
  private def localCorrect(startLeft: Int, startRight: Int, dir: Int, leftWords: IndexedSeq[Selection], rightWords: IndexedSeq[Selection],
                          leftOfMatches: Set[Selection], rightOfMatches: Set[Selection]) = {
    var l = startLeft
    var r = startRight
    val builder = IndexedSeq.newBuilder[WordMatch]
    var done = false
    while(0 <= l && l < leftWords.size && 0 <= r && r < rightWords.size && !done) {
      val left = leftWords(l)
      val right = rightWords(r)
      if (left.toText ==== right.toText && !leftOfMatches.contains(left) && !rightOfMatches.contains(right)) {
        discard(builder += WordMatch(left, right))
      } else {
        done = true
      }
      l += dir; r += dir
    }
    builder.result()
  }

  private def ll(matches: Traversable[WordMatch]) = matches.map(_.left).toSet
  private def rr(matches: Traversable[WordMatch]) = matches.map(_.right).toSet

  def correct(left: FullText, right: FullText, alignment: UnambiguousWordAlignment): UnambiguousWordAlignment = {
    val sortedMatches = alignment.matches.toIndexedSeq.sortBy(_.left) // is sorting important
    val leftWords = Wordizer.toWordIndices(left.s)
    val rightWords = Wordizer.toWordIndices(right.s)
    val leftOfMatches = ll(sortedMatches)
    val rightOfMatches = rr(sortedMatches)
    val atStartRes = localCorrect(0, 0, 1, leftWords, rightWords, leftOfMatches, rightOfMatches)
    val atEndRes = localCorrect(leftWords.length - 1, rightWords.length - 1, -1, leftWords, rightWords, leftOfMatches, rightOfMatches)
    val middleRes = sortedMatches.flatMap { m =>
      val maybeLeftIdx = leftWords.search(m.left)(Ordering.by(_.absoluteFrom))
      val maybeRightIdx = rightWords.search(m.right)(Ordering.by(_.absoluteFrom))
      (maybeLeftIdx, maybeRightIdx) match {
        case (Found(leftIdx), Found(rightIdx)) =>
          localCorrect(leftIdx + 1, rightIdx + 1, 1, leftWords, rightWords, leftOfMatches, rightOfMatches) ++
            localCorrect(leftIdx - 1, rightIdx - 1, -1, leftWords, rightWords, leftOfMatches, rightOfMatches)
        case other => fail(s"match $m is not found in texts: $other")
      }
    }
    val leftResolvedNew = (middleRes ++ atStartRes ++ atEndRes).groupBy(_.left).values.flatMap(_.headOption)
    val resolvedNew = leftResolvedNew.groupBy(_.right).values.flatMap(_.headOption)
    UnambiguousWordAlignment(alignment.matches ++ resolvedNew)
  }
}
