package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.MetricCalculator.ContextIsClosest
import com.kristofszilagyi.sedito.common.{FullText, WordIndexRange, Wordizer}
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class BeforeAfterBestContextMetricsTest extends FreeSpecLike {
  private def constructResultFromMatchingSymmetric(expectedClosestMatchesBefore: Set[(Int, Int)],
                                          expectedClosestMatchesAfter: Set[(Int, Int)],
                                          leftWords: IndexedSeq[WordIndexRange], rightWords: IndexedSeq[WordIndexRange]) = {
    (leftWords.zipWithIndex flatMap { case (l, li) =>
      rightWords.zipWithIndex map { case (r, ri) =>
        val (before, after) = (expectedClosestMatchesBefore.contains((li, ri)), expectedClosestMatchesAfter.contains((li, ri)))
        (l.toSelection.from.i, r.toSelection.from.i,
          ContextIsClosest(beforeFromLeft = before, beforeFromRight = before, afterFromLeft = after, afterFromRight = after))
      }
    }).sortBy(r => (r._1, r._2))
  }

  private def findClosestMatches(left: String, right: String) = {
    val result = MetricCalculator.calcAlignerMetrics(FullText(left), FullText(right)).map(m => (m.leftWord.from.i, m.rightWord.from.i, m.fullClosest))
    result.sortBy{case (l, r, _) => (l, r)}
  }
  private def testBestMatchingLine(left: String, right: String, expectedClosestMatchesBefore: Set[(Int, Int)],
                                   expectedClosestMatchesAfter: Set[(Int, Int)]) = {
    findClosestMatches(left, right) shouldBe constructResultFromMatchingSymmetric(expectedClosestMatchesBefore, expectedClosestMatchesAfter,
      leftWords = Wordizer.toWordIndices(left), rightWords = Wordizer.toWordIndices(right))
  }
  "best matching before-context test vanilla" in {
                //0123456789023456
    val left = """alma alma1 alma2""".stripMargin

    val right = left
    testBestMatchingLine(left, right, expectedClosestMatchesBefore = Set((0, 0), (1,1), (2, 2)), expectedClosestMatchesAfter = Set((0, 0), (1,1), (2, 2)))
  }
}
