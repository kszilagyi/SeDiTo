package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.{FullText, Wordizer}
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
final class MetricCalculatorTest extends FreeSpecLike{

  private val line = "How do you do, my darling? Have you had breakfast yet?"
  private val words = Wordizer.toWordIndices(line)


  "empty context" in {
    MetricCalculator.context(7, words, 0) shouldBe ""
  }

  "negative context works (word edge)" in {
    MetricCalculator.context(6, words, -5) shouldBe "do,my"
  }

  "negative context works (not word edge)" in {
    MetricCalculator.context(6, words, -6) shouldBe "youdo,my"
  }

  "positive context works (word edge)" in {
    MetricCalculator.context(6, words, 5) shouldBe "?Have"
  }

  "positive context works (not word edge)" in {
    MetricCalculator.context(6, words, 6) shouldBe "?Haveyou"
  }

  private def constructResultFromMatching(matches: Set[(Int, Int)], leftNumOfLines: Int, rightNumOfLines: Int): Seq[(Int, Int, Boolean)] = {
    ((0 until leftNumOfLines) flatMap { l =>
      (0 until rightNumOfLines) map { r =>
        if (matches.contains((l, r))) (l, r, true)
        else (l, r, false)
      }
    }).sorted
  }

  private def testBestMatchingLine(left: String, right: String, expectedClosestMatches: Set[(Int, Int)]) = {
    val result = MetricCalculator.calcAlignerMetrics(FullText(left), FullText(right)).map(m => (m.leftLineIdx, m.rightLineIdx, m.lineIsClosestMatchInText))
    result.map{case (l, r, m) => (l.i, r.i, m)}.sorted shouldBe constructResultFromMatching(expectedClosestMatches,
      leftNumOfLines = left.count(_ ==== '\n'), rightNumOfLines = right.count(_ ==== '\n'))
  }
  "best matching line test vanilla" in {
    val left =
      """alma
        |alma1
        |alma2
      """.stripMargin

    val right = left
    testBestMatchingLine(left, right, expectedClosestMatches = Set((0, 0), (1,1), (2, 2)))
  }

  "best matching line test with duplicates (on left)" in {
    val left =
      """alma
        |alma1
        |alma2
        |alma
      """.stripMargin

    val right = """alma
                  |alma1
                  |alma2
                """.stripMargin
    testBestMatchingLine(left, right, expectedClosestMatches = Set((0, 0), (1, 1), (2, 2), (3, 0)))
  }

  "best matching line test with duplicates (on right)" in {
    val left =
      """alma
        |alma1
        |alma2
      """.stripMargin

    val right = """alma
                   |alma1
                   |alma2
                   |alma
                 """.stripMargin
    testBestMatchingLine(left, right, expectedClosestMatches = Set((0, 0), (1, 1), (2, 2), (0, 3)))
  }
}
