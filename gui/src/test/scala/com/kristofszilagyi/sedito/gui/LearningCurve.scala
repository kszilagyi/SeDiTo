package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.aligner.Aligner
import com.kristofszilagyi.sedito.gui.TrainAndDiff.{readDataSetAndMeasureMetrics, readTestCase, testDirs}

object LearningCurve{
  def main(args: Array[String]): Unit = {
    val testCases = testDirs.map(dir => dir -> readTestCase(dir))

    val samples = readDataSetAndMeasureMetrics()
    val testSamples = samples.takeRight(samples.size / 2)

    val testTestCases = testCases.takeRight(testCases.size / 2)

    val learningCurve = (1 to samples.size).par map { size =>
      val trainingSamples = samples.take(size)
      val trainingTestCases = testCases.take(size)
      val (classifier, scaler) = Train.train(trainingSamples, testSamples, logStats = false)
      val aligner = new Aligner(classifier, scaler)

      val trainingResults = WholeAlgorithmMeasurer.measure(aligner, trainingTestCases).map(_._2).reduce(_ + _)
      val testResults = WholeAlgorithmMeasurer.measure(aligner, testTestCases).map(_._2).reduce(_ + _)
      size -> (trainingResults.f1, testResults.f1)
    }

  }
}
