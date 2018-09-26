package com.kristofszilagyi.sedito.gui

import java.awt.Color
import java.nio.file.Path
import java.time.{Duration, Instant}

import com.kristofszilagyi.sedito.aligner.Aligner
import com.kristofszilagyi.sedito.common.TestCase
import com.kristofszilagyi.sedito.gui.TrainAndDiff.{readDataSetAndMeasureMetrics, readTestCase, testDirs}
import org.log4s.getLogger
import smile.plot
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._

object LearningCurve{
  private val logger = getLogger

  private def withMetrics(testCases: Seq[(Path, TestCase)], samples: List[(Path, Samples)]) = {
    assert(testCases.size ==== samples.size)
    testCases.zip(samples).map{ case ((path1, testCase), (path2, sample)) =>
      assert(path1 ==== path2)
      (path1, testCase, sample.metricsWithResults.map(_.metrics))
    }
  }
  def main(args: Array[String]): Unit = {
    logger.info("Start")
    val start = Instant.now()

    val testCases = testDirs.map(dir => dir -> readTestCase(dir))

    val samples = readDataSetAndMeasureMetrics()
    val testSamples = samples.takeRight(samples.size / 2)

    val testTestCases = testCases.takeRight(testCases.size / 2)

    val learningCurve = ((1 to samples.size / 2).par map { size =>
      logger.info(s"Doing size: $size")
      val trainingSamples = samples.take(size)
      val trainingTestCases = testCases.take(size)
      val (classifier, scaler) = Train.train(trainingSamples, testSamples, logStats = false)
      val aligner = new Aligner(classifier, scaler)

      val trainingResults = WholeAlgorithmMeasurer.measureFast(aligner, withMetrics(trainingTestCases, trainingSamples)).aggregate
      val testResults = WholeAlgorithmMeasurer.measureFast(aligner, withMetrics(testTestCases, testSamples)).aggregate
      logger.info(s"Finished size: $size")
      size -> ((trainingResults.f1, testResults.f1))
    }).seq

    logger.info(learningCurve.mkString("\n"))
    val trainCoords = learningCurve.map { case (size, (train, _)) =>
      Array(size.toDouble, train)
    }.toArray
    val testCoords = learningCurve.map { case (size, (_, test)) =>
      Array(size.toDouble, test)
    }.toArray


    val _ = plot.plot(data = trainCoords ++ testCoords, label = trainCoords.map(_ => 0) ++ testCoords.map(_ => 1),
      palette = Array(Color.BLUE, Color.RED), legend = Array('x', 'x'))
    val duration = Duration.between(start, Instant.now())
    logger.info(s"Took: ${duration.toMinutes} minutes, ${duration.toMillis/1000 - duration.toMinutes * 60} seconds")
  }
}
