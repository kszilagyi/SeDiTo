package com.kristofszilagyi.sedito.gui

import scala.io.Source
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.gui.Analyzer.load
import smile.plot
object Analyzer {
  final case class ResultToAnalyze(predicted: Seq[Double], expected: Seq[Int]) {
    def predictedArray: Array[Int] = predicted.map(_.round.toInt).toArray
    def expectedArray: Array[Int] = expected.toArray
    def misPredicted: Seq[(Double, Int)] = {
      predicted.zip(expected).filter{ case (pred, exp) =>
        val predRounded = pred.round.toInt
        predRounded !=== exp
      }
    }
  }

  def load(baseFilename: String): ResultToAnalyze = {
    val predictedLines = Source.fromFile(Write.predictedPath(baseFilename).toString).getLines().toVector
    val expectedLines = Source.fromFile(Write.expectedPath(baseFilename).toString).getLines().toVector
    ResultToAnalyze(predicted = predictedLines.map(_.toDouble), expected = expectedLines.map(_.toInt))
  }

}

object ShowMisPredHistogram {
  def main(args: Array[String]): Unit = {
    val train = load("training")
    val test = load("test")
    val trainMisPred = train.misPredicted
    val testMisPred = test.misPredicted
    val wTrain = plot.hist(trainMisPred.map(_._1).toArray, 100)
    wTrain.canvas.setTitle("Train")
    val wTest = plot.hist(testMisPred.map(_._1).toArray, 100)
    discard(wTest.canvas.setTitle("Test"))
  }
}

