package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.{FullText, Wordizer}
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._

final class Pass1FeatureCalculatorTest extends FreeSpecLike{
  private val line = "How do you do, my darling? Have you had breakfast yet?"
  private val words = Wordizer.toWordIndices(line)

  "empty context" in {
    Pass1FeatureCalculator.context(7, words, 0) shouldBe ""
  }

  "negative context works (word edge)" in {
    Pass1FeatureCalculator.context(6, words, -5) shouldBe "do,my"
  }

  "negative context works (not word edge)" in {
    Pass1FeatureCalculator.context(6, words, -6) shouldBe "youdo,my"
  }

  "positive context works (word edge)" in {
    Pass1FeatureCalculator.context(6, words, 5) shouldBe "?Have"
  }

  "positive context works (not word edge)" in {
    Pass1FeatureCalculator.context(6, words, 6) shouldBe "?Haveyou"
  }

  private def constructResultFromMatching(matches: Set[(Int, Int)], leftNumOfLines: Int, rightNumOfLines: Int): Seq[(Int, Int, Boolean)] = {
    ((0 until leftNumOfLines) flatMap { l =>
      (0 until rightNumOfLines) map { r =>
        if (matches.contains((l, r))) (l, r, true)
        else (l, r, false)
      }
    }).sorted
  }

  private def findClosestMatches(left: String, right: String) = {
    val result = Pass1FeatureCalculator.calcAlignerFeatures(FullText(left), FullText(right)).map(m => (m.leftLineIdx, m.rightLineIdx, m.lineIsClosestMatchInText))
    result.map{case (l, r, m) => (l.i, r.i, m)}.toSeq.sorted
  }
  private def testBestMatchingLine(left: String, right: String, expectedClosestMatches: Set[(Int, Int)]) = {
    findClosestMatches(left, right) shouldBe constructResultFromMatching(expectedClosestMatches,
      leftNumOfLines = left.count(_ ==== '\n'), rightNumOfLines = right.count(_ ==== '\n'))
  }
  "best matching line test vanilla" in {
    val left =
      """alma
        |alma1
        |alma2
      """.stripMargin

    val right = left
    findClosestMatches(left, right) shouldBe Vector((0, 0, true), (1,1, true), (2, 2, true))
  }

  "best matching line test approximation" in {
    val left =
      """almaalmaalmaalma
        |almaalmaalma
        |almaalma
      """.stripMargin

    val right =  """almaalmaalmaalma1
                   |almaalmaalma1
                   |almaalma1
                 """.stripMargin

    findClosestMatches(left, right) shouldBe Vector((0,0,true), (0,1,false), (1,0,false), (1,1,true), (1,2,false), (2,0,false), (2,1,false), (2,2,true))
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
    testBestMatchingLine(left, right, expectedClosestMatches = Set((1, 1), (2, 2)))
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
    testBestMatchingLine(left, right, expectedClosestMatches = Set((1, 1), (2, 2)))
  }

  "best matching line test when it's not symmetrical (options on the left)" in {
    val left =
      """almamama1
         |almamama12
      """.stripMargin

    val right = """almamama
                """.stripMargin
    testBestMatchingLine(left, right, expectedClosestMatches = Set((0, 0)))
  }

  "best matching line test when it's not symmetrical (options on the right)" in {

    val left = """almamama
                """.stripMargin

    val right =
      """almamama1
        |almamama12
      """.stripMargin

    testBestMatchingLine(left, right, expectedClosestMatches = Set((0, 0)))
  }

  "best matching line test two words (other words all different)" in {
    val left =
      """alma aaa
        |alma1 bbb
        |alma2 ccc
      """.stripMargin

    val right =  """alma ddd
                   |alma1 eee
                   |alma2 fff
                 """.stripMargin
    testBestMatchingLine(left, right, expectedClosestMatches = Set((0, 0), (1,1), (2, 2)))
  }

  "best matching line test two words (other words same)" in {
    val left =
      """alma aaa
        |alma1 bbb
        |alma2 ccc
      """.stripMargin

    val right = left
    findClosestMatches(left, right) shouldBe
      Vector((0,0,true), (0,0,true), (1,1,true), (1,1,true), (2,2,true), (2,2,true))
  }

  "best matching line test 3 words (other words same)" in {
    val left =
      """alma aaa ddd
        |alma1 bbb eee
        |alma2 ccc fff
      """.stripMargin

    val right = left
    findClosestMatches(left, right) shouldBe
      Vector((0,0,true), (0,0,true), (0,0,true), (1,1,true), (1,1,true), (1,1,true),
        (2,2,true), (2,2,true), (2,2,true))
  }

  "same word in line twice" in {
    val left =
      """alma alma
        |alma1 bbb
        |alma2 ccc
      """.stripMargin

    val right = left
    findClosestMatches(left, right) shouldBe
      Vector((0,0,true), (0,0,true), (1,1,true), (1,1,true), (2,2,true), (2,2,true))
  }
}
