package com.kristofszilagyi.sedito.gui

import java.nio.file.Path
import java.time.{Duration, Instant}

import com.kristofszilagyi.sedito.aligner.Aligner
import com.kristofszilagyi.sedito.common.TestCase
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.gui.TrainAndDiff.{readDataSetAndMeasureMetrics, readTestCase, testDirs}
import org.log4s.getLogger
import smile.data.{AttributeDataset, NominalAttribute, NumericAttribute}
import smile.write

import scala.util.Random

object LearningCurve{
  private val logger = getLogger

  private def withMetrics(testCases: Seq[(Path, TestCase)], samples: List[(Path, Samples)]) = {
    assert(testCases.size ==== samples.size)
    testCases.zip(samples).map{ case ((path1, testCase), (path2, sample)) =>
      assert(path1 ==== path2, s"$path1 == $path2")
      (path1, testCase, sample.metricsWithResults.map(_.metrics))
    }
  }
  private def oneRandomCurve(random: Random, samples: List[(Path, Samples)], testCases: Seq[(Path, TestCase)]) = {
    assert(samples.size ==== testCases.size)
    val half = samples.size / 2

    val (shuffledSamples, shuffledTestCases) = random.shuffle(samples.zip(testCases)).unzip

    val testSamples = shuffledSamples.takeRight(half)
    val testTestCases = shuffledTestCases.takeRight(half)

    val learningCurve = ((1 to 5).par map { size =>
      logger.info(s"Doing size: $size")
      val trainingSamples = shuffledSamples.take(size)
      val trainingTestCases = shuffledTestCases.take(size)
      val (classifier, scaler) = Train.train(trainingSamples, testSamples, logStats = false)
      val aligner = new Aligner(classifier, scaler)

      val trainingResults = WholeAlgorithmMeasurer.measureFast(aligner, withMetrics(trainingTestCases, trainingSamples)).aggregate
      val testResults = WholeAlgorithmMeasurer.measureFast(aligner, withMetrics(testTestCases, testSamples)).aggregate
      logger.info(s"Finished size: $size")
      size -> ((trainingResults.f1, testResults.f1))
    }).seq

    learningCurve
  }
  private def toCoords(data: Traversable[(Int, (Double, Double))]) = {
    val trainCoords = data.map { case (size, (train, _)) =>
      Array(size.toDouble, train)
    }.toArray
    val testCoords = data.map { case (size, (_, test)) =>
      Array(size.toDouble, test)
    }.toArray

    val attrs = new AttributeDataset("coords", Array(new NumericAttribute("x"), new NumericAttribute("y")),
      new NominalAttribute("isTraining", Array("false", "true")))
    trainCoords.foreach { xs =>
      attrs.add(xs, 1)
    }

    testCoords.foreach { xs =>
      attrs.add(xs, 0)
    }
    attrs
  }
  def main(args: Array[String]): Unit = {
    logger.info("Start")
    val start = Instant.now()

    val testCases = testDirs.map(dir => dir -> readTestCase(dir))

    val samples = readDataSetAndMeasureMetrics()
    val random = new Random(125)
    val learningCurves = (1 to 2).map(_ => oneRandomCurve(random, samples, testCases))

    val flattenedLearningCurves = learningCurves.flatten
    val flattenedCoords = toCoords(flattenedLearningCurves)

    write.arff(flattenedCoords, "flattened_learning_curve.arff")

    val avgLearningCurve = flattenedLearningCurves.groupBy(_._1).map { case(runs, results) =>
      val f1Sums = results.map(_._2).reduce[(Double, Double)]{ case (r1, r2) =>
        (r1._1 + r2._1, r1._2 + r2._2)
      }
      val cnt = results.size
      runs -> ((f1Sums._1 / cnt, f1Sums._2 / cnt))
    }
    val avgCoords = toCoords(avgLearningCurve)
    write.arff(avgCoords, "avg_learning_curve.arff")

    val duration = Duration.between(start, Instant.now())
    logger.info(s"Took: ${duration.toMinutes} minutes, ${duration.toMillis/1000 - duration.toMinutes * 60} seconds")
  }
}
