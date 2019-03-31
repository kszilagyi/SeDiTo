package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.LineIdx
import com.kristofszilagyi.sedito.common.Warts._
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class IndentationCalculatorTest extends FreeSpecLike {
  "all 0" in {
    val leftLines = 30
    val rightLines = 40
    val leftIdx = LineIdx(10)
    val rightIdx = LineIdx(15)
    val indent = new IndentationCalculator(Vector.fill(leftLines)("import"), Vector.fill(rightLines)("import")).calcIndentation(leftIdx, rightIdx)
    indent shouldBe Indentation(
      IndentationOneSide(Seq.fill(15)(0), leftIdx, 19),
      IndentationOneSide(Seq.fill(15)(0), rightIdx, 24),
    )
  }

  "all 0 beginning" in {
    val leftLines = 30
    val rightLines = 40
    val leftIdx = LineIdx(0)
    val rightIdx = LineIdx(1)
    val indent = new IndentationCalculator(Vector.fill(leftLines)(" import"), Vector.fill(rightLines)(" import")).calcIndentation(leftIdx, rightIdx)
    indent shouldBe Indentation(
      IndentationOneSide(Seq.fill(7)(0) ++ Seq.fill(8)(1), leftIdx, 29),
      IndentationOneSide(Seq.fill(6)(0) ++ Seq.fill(9)(1), rightIdx, 38),
    )
  }

  "all 0 end" in {
    val leftLines = 30
    val rightLines = 40
    val leftIdx = LineIdx(29)
    val rightIdx = LineIdx(38)
    val indent = new IndentationCalculator(Vector.fill(leftLines)(" import"), Vector.fill(rightLines)(" import")).calcIndentation(leftIdx, rightIdx)
    indent shouldBe Indentation(
      IndentationOneSide(Seq.fill(8)(1) ++ Seq.fill(7)(0), leftIdx, 0),
      IndentationOneSide(Seq.fill(9)(1) ++ Seq.fill(6)(0), rightIdx, 1),
    )
  }

  private val leftLines = """import scala.io.Source0
                        |import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._1
                        |import com.kristofszilagyi.sedito.common.Warts2
                        |import com.kristofszilagyi.sedito.common.Warts._3
                        |import com.kristofszilagyi.sedito.gui.Analyzer.load4
                        |import smile.plot5
                        |
                        |object Analyzer {7
                        |  final case class ResultToAnalyze(predicted: Seq[Double], expected: Seq[Int]) {8
                        |    def predictedArray: Array[Int] = predicted.map(_.round.toInt).toArray9
                        |
                        |    def misPredicted: Seq[(Double, Int)] = {11
                        |      predicted.zip(expected).filter{ case (pred, exp) =>12
                        |        val predRounded = pred.round.toInt13
                        |        predRounded !=== exp14
                        |      }15
                        |    }16
                        |  }17
                        |
                        |  @SuppressWarnings(Array(Warts.ToString))19
                        |  def load(baseFilename: String): ResultToAnalyze = {20
                        |    val predictedLines = Source.fromFile(Write.predictedPath(baseFilename).toString).getLines().toVector21
                        |
                        |    ResultToAnalyze(predicted = predictedLines.map(_.toDouble), expected = expectedLines.map(_.toInt))23
                        |  }24
                        |
                        |}26""".stripMargin.lines.toVector

  private val rightLines = """import scala.io.Source0
                            |import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._1
                            |import com.kristofszilagyi.sedito.common.Warts2
                            |import com.kristofszilagyi.sedito.common.Warts._3
                            |import com.kristofszilagyi.sedito.gui.Analyzer.load4
                            |import smile.plot5
                            |
                            |object Analyzer {7
                            |  final case class ResultToAnalyze(predicted: Seq[Double], expected: Seq[Int]) {8
                            |    def predictedArray: Array[Int] = predicted.map(_.round.toInt).toArray9
                            |    def expectedArray: Array[Int] = expected.toArray10
                            |    def misPredicted: Seq[(Double, Int)] = {11
                            |      predicted.zip(expected).filter{ case (pred, exp) =>12
                            |
                            |        predRounded !=== exp14
                            |      }15
                            |    }16
                            |  }17
                            |
                            |  @SuppressWarnings(Array(Warts.ToString))19
                            |  def load(baseFilename: String): ResultToAnalyze = {20
                            |
                            |    val expectedLines = Source.fromFile(Write.expectedPath(baseFilename).toString).getLines().toVector22
                            |    ResultToAnalyze(predicted = predictedLines.map(_.toDouble), expected = expectedLines.map(_.toInt))23
                            |  }24
                            |
                            |}26""".stripMargin.lines.toVector

  "middle non-trivial" in {
    val leftIdx = LineIdx(15)
    val rightIdx = LineIdx(17)
    val indent = new IndentationCalculator(leftLines, rightLines).calcIndentation(leftIdx, rightIdx)
    discard(indent.left.indents shouldBe Seq(0, 2, 4, 4, 6, 8, 8, 6, 4, 2, 2, 2, 4, 4, 2))
    indent.right.indents shouldBe Seq(4, 4, 4, 6, 8, 6, 4, 2, 2, 2, 4, 4, 2, 0, 0)
  }
}
