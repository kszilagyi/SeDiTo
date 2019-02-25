package com.kristofszilagyi.sedito.gui

import java.nio.file.Paths

import com.kristofszilagyi.sedito.aligner.Pass1MetricCalculator._
import com.kristofszilagyi.sedito.aligner.Pass1Result
import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import com.kristofszilagyi.sedito.common.{CharIdxInLine, LineIdx, Selection}
import com.kristofszilagyi.sedito.gui.TrainPass2._
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class TestPass2 extends FreeSpecLike {

  private val lines = List(
    List("word1", "word2"),
    List("word3")
  )

  private val lineTexts = lines.map(_.mkString(" "))
  private val fullText = lineTexts.mkString("\n")
  private val pairwiseFeatures = PairwiseFeatures(1, 1, 1, 1, 1)
  private val contextFeatures = ContextFeatures(pairwiseFeatures, pairwiseFeatures)
  private val contextIsClosest = ContextIsClosest(beforeFromLeft = true, beforeFromRight = true, afterFromLeft = true, afterFromRight = true)
  private val pass1Features = Pass1Features(
    Phase1Features(selection(0, 0), selection(0, 0), leftContainsRight = true, rightContainsLeft = true, pairwiseFeatures, pairwiseFeatures, pairwiseFeatures,
      contextFeatures, contextFeatures, contextFeatures, contextFeatures, contextFeatures, contextFeatures, LineIdx(1), LineIdx(2)),
    lineIsClosestMatchInText = true,
    contextIsClosest, contextIsClosest, contextIsClosest, contextIsClosest, contextIsClosest, contextIsClosest
  )

  private def selection(lineIdx: Int, wordIdx: Int) = {
    val line = lineTexts(lineIdx)
    val word = lines(lineIdx)(wordIdx)
    val from = line.indexOf(word)
    val to = from + word.length
    Selection.create(line, LineIdx(lineIdx), CharIdxInLine(from), CharIdxInLine(to), fullText.indexOf(word)).getAssert
  }

  "creating metrics work" in {
    val path = Paths.get(".")

    val combinedResults = List(
      Pass1ResultWithTruth(
        Pass1Result(selection(0, 0), selection(0, 0), probability = 1),
        pass1Features,
        shouldBeMatching = true,
      ) -> LineFeatures(sum = 2, avg = 1),
      Pass1ResultWithTruth(
        Pass1Result(selection(0, 1), selection(0, 0), probability = 0),
        pass1Features,
        shouldBeMatching = false
      ) -> LineFeatures(sum = 2, avg = 1),
      Pass1ResultWithTruth(
        Pass1Result(selection(0, 0), selection(0, 1), probability = 0),
        pass1Features,
        shouldBeMatching = false
      ) -> LineFeatures(sum = 2, avg = 1),
      Pass1ResultWithTruth(
        Pass1Result(selection(0, 1), selection(0, 1), probability = 1),
        pass1Features,
        shouldBeMatching = true
      ) -> LineFeatures(sum = 2, avg = 1),
      Pass1ResultWithTruth(
        Pass1Result(selection(1, 0), selection(1, 0), probability = 1),
        pass1Features,
        shouldBeMatching = true
      ) -> LineFeatures(sum = 1, avg = 1),
      Pass1ResultWithTruth(
        Pass1Result(selection(1, 0), selection(0, 0), probability = 0),
        pass1Features,
        shouldBeMatching = false
      ) -> LineFeatures(sum = 0.5, avg = 1.0/3.0),
      Pass1ResultWithTruth(
        Pass1Result(selection(1, 0), selection(0, 1), probability = 0.5),
        pass1Features,
        shouldBeMatching = false
      ) -> LineFeatures(sum = 0.5, avg = 1.0/3.0),
      Pass1ResultWithTruth(
        Pass1Result(selection(0, 0), selection(1, 0), probability = 0),
        pass1Features,
        shouldBeMatching = false
      ) -> LineFeatures(sum = 0.5, avg = 1.0/3.0),
      Pass1ResultWithTruth(
        Pass1Result(selection(0, 1), selection(1, 0), probability = 0.5),
        pass1Features,
        shouldBeMatching = false
      ) -> LineFeatures(sum = 0.5, avg = 1.0/3.0)
    )

    val (pass1Results, _) = combinedResults.unzip

    val pass1ResultsAndPath = List(
      PathAndPass1Results(
        path,
        pass1Results
      )
    )

    val expected = List(
      PathAndPass2Samples(
        path,
        Pass2Samples(
          combinedResults.map { case (pass1, pass2) =>
            Pass2FeaturesWithResults(
              Pass2Features(
                pass1.pass1Result,
                pass1Features,
                pass2
              ),
              pass1.shouldBeMatching
            )
          }
        )
      )
    )
    TrainPass2.calcPass2Features(pass1ResultsAndPath) shouldBe expected
  }
}
