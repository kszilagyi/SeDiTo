package com.kristofszilagyi.sedito.gui

import java.nio.file.Path
import java.time.{Duration, Instant}

import com.kristofszilagyi.sedito.aligner.{Aligner, MetricCalculator}
import com.kristofszilagyi.sedito.aligner.MetricCalculator.Metrics
import com.kristofszilagyi.sedito.common.{TestCase, Warts}
import com.kristofszilagyi.sedito.gui.TrainAndDiff._
import org.log4s.getLogger

//todo add cross-validation and maximum difference
/**
  * Measures overall performance not only classifier performance
  */
object WholeAlgorithmMeasurer {
  final case class Results(tp: Long, fp: Long, fn: Long, selPos: Long, expectedPos: Long) {
    def +(other: Results): Results = {
      Results(tp = tp + other.tp, fp = fp + other.fp, fn = fn + other.fn,
        selPos = selPos + other.selPos, expectedPos = expectedPos + other.expectedPos)
    }
    def recall: Double = {
      tp.toDouble / (tp + fn)
    }
    def precision: Double = {
      tp.toDouble / (tp + fp)
    }
    def f1: Double = {
      2 * (precision * recall) / (precision + recall)
    }

    def resultString: String = {
      f"f1: $f1%.5f, tp: $tp%4d, fp: $fp%2d, fn: $fn%2d, selPos: $selPos%4d, expectedPos: $expectedPos%4d, total mispred: ${fp + fn}%2d"
    }
  }
  final case class MultiResult(results: Seq[(Path, Results)]) {
    def aggregate: Results = results.map(_._2).reduce(_ + _)

    @SuppressWarnings(Array(Warts.ToString))
    def nestedResultString: String = {
      val resultLines = results.sortBy(_._2.f1).map { case (path, result) =>
        path.getFileName.toString.padTo(34, " ").mkString + s": ${result.resultString}"
      }
      resultLines.mkString("\n")
    }
  }
  private val logger = getLogger

  def measure(aligner: Aligner, testCases: Seq[(Path, TestCase)]): MultiResult = {
    measureFast(aligner, testCases.map { case (path, testCase) =>
      (path, testCase, MetricCalculator.calcAlignerMetrics(testCase.left, testCase.right))
    })
  }

  def measureFast(aligner: Aligner, testCases: Seq[(Path, TestCase, IndexedSeq[Metrics])]): MultiResult = {
    MultiResult(testCases.map { case (path, testCase, metrics) =>
      val actual = aligner.alignFast(metrics).matches
      val expected = testCase.wordAlignment.toUnambigous.matches
      val tp = actual.intersect(expected).size
      val fp = actual.size - tp
      val fn = (expected -- actual).size
      path -> Results(tp = tp.toLong, fp = fp.toLong, fn = fn.toLong, actual.size.toLong, expected.size.toLong)
    })
  }

  def main(args: Array[String]): Unit = {
    logger.info("Start")
    val start = Instant.now()
    val testCases = testDirs.map(dir => dir -> readTestCase(dir))
    val (classifier, scaler) = Main.loadAI()
    val aligner = new Aligner(classifier, scaler)
    val (training, test) = testCases.splitAt(testCases.size / 2)
    val nestedTrainingResults = measure(aligner, training)
    val trainigResults = nestedTrainingResults.aggregate
    val nestedTestResults = measure(aligner, test)
    val testResults = nestedTestResults.aggregate

    logger.info(s"Training results: ${trainigResults.resultString}")
    logger.info(s"Test results: ${testResults.resultString}")
    logger.info(s"Training: \n${nestedTrainingResults.nestedResultString}")
    logger.info(s"Test: \n${nestedTestResults.nestedResultString}")
    val duration = Duration.between(start, Instant.now())
    logger.info(s"Took: ${duration.toMinutes} minutes, ${duration.toMillis/1000 - duration.toMinutes * 60} seconds")
  }

}
