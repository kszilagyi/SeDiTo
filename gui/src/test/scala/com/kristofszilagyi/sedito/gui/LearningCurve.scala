package com.kristofszilagyi.sedito.gui

import java.awt.Color

import com.kristofszilagyi.sedito.aligner.Aligner
import com.kristofszilagyi.sedito.common.Warts
import com.kristofszilagyi.sedito.gui.TrainAndDiff.{readDataSetAndMeasureMetrics, readTestCase, testDirs}
import org.log4s.getLogger
import smile.plot

object LearningCurve{
  private val logger = getLogger

  def main(args: Array[String]): Unit = {
    logger.info("Start")
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

      @SuppressWarnings(Array(Warts.TraversableOps))
      val trainingResults = WholeAlgorithmMeasurer.measure(aligner, trainingTestCases).map(_._2).reduce(_ + _)
      @SuppressWarnings(Array(Warts.TraversableOps))
      val testResults = WholeAlgorithmMeasurer.measure(aligner, testTestCases).map(_._2).reduce(_ + _)
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
  }
}
