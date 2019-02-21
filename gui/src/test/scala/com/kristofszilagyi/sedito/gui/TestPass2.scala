package com.kristofszilagyi.sedito.gui

import java.nio.file.Paths

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
        shouldBeMatching = true
      ) -> LineMetrics(sum = 2, avg = 1),
      Pass1ResultWithTruth(
        Pass1Result(selection(0, 1), selection(0, 0), probability = 0),
        shouldBeMatching = false
      ) -> LineMetrics(sum = 2, avg = 1),
      Pass1ResultWithTruth(
        Pass1Result(selection(0, 0), selection(0, 1), probability = 0),
        shouldBeMatching = false
      ) -> LineMetrics(sum = 2, avg = 1),
      Pass1ResultWithTruth(
        Pass1Result(selection(0, 1), selection(0, 1), probability = 1),
        shouldBeMatching = true
      ) -> LineMetrics(sum = 2, avg = 1),
      Pass1ResultWithTruth(
        Pass1Result(selection(1, 0), selection(1, 0), probability = 1),
        shouldBeMatching = true
      ) -> LineMetrics(sum = 1, avg = 1),
      Pass1ResultWithTruth(
        Pass1Result(selection(1, 0), selection(0, 0), probability = 0),
        shouldBeMatching = false
      ) -> LineMetrics(sum = 0, avg = 0),
      Pass1ResultWithTruth(
        Pass1Result(selection(1, 0), selection(0, 1), probability = 0.5),
        shouldBeMatching = false
      ) -> LineMetrics(sum = 0.5, avg = 0.5),
      Pass1ResultWithTruth(
        Pass1Result(selection(0, 0), selection(1, 0), probability = 0),
        shouldBeMatching = false
      ) -> LineMetrics(sum = 0, avg = 0),
      Pass1ResultWithTruth(
        Pass1Result(selection(0, 1), selection(1, 0), probability = 0.5),
        shouldBeMatching = false
      ) -> LineMetrics(sum = 0.5, avg = 0.5)
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
            Pass2MetricsWithResults(
              Pass2Metrics(
                pass1.pass1Result,
                pass2
              ),
              pass1.shouldBeMatching
            )
          }
        )
      )
    )
    TrainPass2.calcPass2Metrics(pass1ResultsAndPath) shouldBe expected
  }
}
