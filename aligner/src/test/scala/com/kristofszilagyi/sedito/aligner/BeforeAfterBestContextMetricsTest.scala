package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.MetricCalculator.ContextIsClosest
import com.kristofszilagyi.sedito.common.{FullText, Warts, WordIndexRange, Wordizer}
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class BeforeAfterBestContextMetricsTest extends FreeSpecLike {
  private def constructResultFromMatchingSymmetric(
                                          expectedClosestMatchesBeforeFromLeft: Set[(Int, Int)],
                                          expectedClosestMatchesBeforeFromRight: Set[(Int, Int)],
                                          expectedClosestMatchesAfterFromLeft: Set[(Int, Int)],
                                          expectedClosestMatchesAfterFromRight: Set[(Int, Int)],
                                          leftWords: IndexedSeq[WordIndexRange], rightWords: IndexedSeq[WordIndexRange],
                                          differentLeftWords: Set[Int], differentRightWords: Set[Int]) = {
    (leftWords.zipWithIndex.filterNot{case (_, i) => differentLeftWords.contains(i)} flatMap { case (l, li) =>
      rightWords.zipWithIndex.filterNot{case (_, i) => differentRightWords.contains(i)} map { case (r, ri) =>
        val beforeFromLeft = expectedClosestMatchesBeforeFromLeft.contains((li, ri))
        val afterFromLeft = expectedClosestMatchesAfterFromLeft.contains((li, ri))
        val beforeFromRight = expectedClosestMatchesBeforeFromRight.contains((li, ri))
        val afterFromRight = expectedClosestMatchesAfterFromRight.contains((li, ri))
        (l.toSelection.from.i, r.toSelection.from.i,
          ContextIsClosest(beforeFromLeft = beforeFromLeft, beforeFromRight = beforeFromRight, afterFromLeft = afterFromLeft, afterFromRight = afterFromRight))
      }
    }).sortBy(r => (r._1, r._2))
  }

  private def findClosestMatches(left: String, right: String) = {
    val result = MetricCalculator.calcAlignerMetrics(FullText(left), FullText(right)).map(m => (m.leftWord.from.i, m.rightWord.from.i, m.fullClosest))
    result.sortBy{case (l, r, _) => (l, r)}
  }
  @SuppressWarnings(Array(Warts.DefaultArguments))
  private def testBestMatchingLine(left: String, right: String, expectedClosestMatchesBefore: Set[(Int, Int)],
                                   expectedClosestMatchesAfter: Set[(Int, Int)], differentLeftWords: Set[Int] = Set.empty,
                                   differentRightWords: Set[Int] = Set.empty) = {
    findClosestMatches(left, right) shouldBe constructResultFromMatchingSymmetric(
      expectedClosestMatchesBeforeFromLeft = expectedClosestMatchesBefore, expectedClosestMatchesBeforeFromRight = expectedClosestMatchesBefore,
      expectedClosestMatchesAfterFromLeft = expectedClosestMatchesAfter, expectedClosestMatchesAfterFromRight = expectedClosestMatchesAfter,
      leftWords = Wordizer.toWordIndices(left), rightWords = Wordizer.toWordIndices(right),
      differentLeftWords = differentLeftWords, differentRightWords = differentRightWords)
  }

  @SuppressWarnings(Array(Warts.DefaultArguments))
  private def testBestMatchingLineAsym(left: String, right: String,
                                   expectedClosestMatchesBeforeFromLeft: Set[(Int, Int)],
                                   expectedClosestMatchesBeforeFromRight: Set[(Int, Int)],
                                   expectedClosestMatchesAfterFromLeft: Set[(Int, Int)],
                                   expectedClosestMatchesAfterFromRight: Set[(Int, Int)],
                                   differentLeftWords: Set[Int] = Set.empty,
                                   differentRightWords: Set[Int] = Set.empty) = {
    findClosestMatches(left, right) shouldBe constructResultFromMatchingSymmetric(
      expectedClosestMatchesBeforeFromLeft = expectedClosestMatchesBeforeFromLeft,
      expectedClosestMatchesBeforeFromRight = expectedClosestMatchesBeforeFromRight,
      expectedClosestMatchesAfterFromLeft = expectedClosestMatchesAfterFromLeft,
      expectedClosestMatchesAfterFromRight = expectedClosestMatchesAfterFromRight,
      leftWords = Wordizer.toWordIndices(left), rightWords = Wordizer.toWordIndices(right), differentLeftWords = differentLeftWords, differentRightWords = differentRightWords)
  }

  "vanilla" in {
    val left = """alma alma1 alma2""".stripMargin

    val right = left
    testBestMatchingLine(left, right, expectedClosestMatchesBefore = Set((0, 0), (1,1), (2, 2)), expectedClosestMatchesAfter = Set((0, 0), (1,1), (2, 2)))
  }

  "order changed" in {
    val left = """alma1 alma alma2""".stripMargin

    val right = left
    testBestMatchingLine(left, right, expectedClosestMatchesBefore = Set((0, 0), (1,1), (2, 2)), expectedClosestMatchesAfter = Set((0, 0), (1,1), (2, 2)))
  }

  "first word deleted - alien word" in {
    val left = """kot alma alma1 alma2""".stripMargin

    val right =  """alma alma1 alma2""".stripMargin
    testBestMatchingLine(left, right, expectedClosestMatchesBefore = Set((1, 0), (2,1), (3, 2)), expectedClosestMatchesAfter = Set((1, 0), (2, 1), (3, 2)),
      differentLeftWords = Set(0))
  }

  "first word deleted - similar word" in {
    val left = """alma3 alma alma1 alma2""".stripMargin

    val right =  """alma alma1 alma2""".stripMargin
    testBestMatchingLineAsym(left, right,
      expectedClosestMatchesBeforeFromLeft = Set((0, 0), (1, 1), (2, 2), (3, 2)),
      expectedClosestMatchesBeforeFromRight = Set((0, 0), (1, 1), (2, 2)),
      expectedClosestMatchesAfterFromLeft = Set((0, 0), (1, 0), (2, 1), (3, 2)),
      expectedClosestMatchesAfterFromRight = Set((1, 0), (2, 1), (3, 2)))
  }

  "last word deleted" in {
    val left = """alma alma1 alma2 kot""".stripMargin

    val right =  """alma alma1 alma2""".stripMargin
    testBestMatchingLine(left, right, expectedClosestMatchesBefore = Set((0, 0), (1, 1), (2, 2)), expectedClosestMatchesAfter = Set((0, 0), (1, 1), (2, 2)),
      differentLeftWords = Set(3))
  }

  "first word added" in {
    val left = """alma alma1 alma2""".stripMargin

    val right =  """kot alma alma1 alma2""".stripMargin
    testBestMatchingLine(left, right, expectedClosestMatchesBefore = Set((0, 1), (1, 2), (2, 3)), expectedClosestMatchesAfter = Set((0, 1), (1, 2), (2, 3)),
      differentRightWords = Set(0))
  }

  "last word added" in {
    val left = """alma alma1 alma2""".stripMargin

    val right =  """alma alma1 alma2 kot""".stripMargin
    testBestMatchingLine(left, right, expectedClosestMatchesBefore = Set((0, 0), (1, 1), (2, 2)), expectedClosestMatchesAfter = Set((0, 0), (1, 1), (2, 2)),
      differentRightWords = Set(3))
  }


}
