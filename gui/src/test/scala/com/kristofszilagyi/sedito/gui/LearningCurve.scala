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
//todo probably should filter out the cases which I do not want to tackle yet and only teach with those which I want
object LearningCurve{
  private val logger = getLogger

  private def keepPathAndSamples(testCases: Seq[(Path, TestCase)], samples: List[(Path, Samples)]) = {
    assert(testCases.size ==== samples.size)
    testCases.zip(samples).map{ case ((path1, _), (path2, sample)) =>
      assert(path1 ==== path2, s"$path1 == $path2")
      (path1, sample)
    }
  }
  private def oneRandomCurve(random: Random, samples: List[(Path, Samples)], testCases: Seq[(Path, TestCase)]) = {
    assert(samples.size ==== testCases.size)
    val trainingSize = (samples.size * Train.trainingRatio).toInt

    val (shuffledSamples, shuffledTestCases) = random.shuffle(samples.zip(testCases)).unzip

    val testSamples = shuffledSamples.takeRight(trainingSize)
    val testTestCases = shuffledTestCases.takeRight(trainingSize)

    val learningCurve = ((1 to (trainingSize, 6)).par map { size =>
      logger.info(s"Doing size: $size")
      val trainingSamples = shuffledSamples.take(size)
      val trainingTestCases = shuffledTestCases.take(size)
      val (classifier, scaler) = Train.train(trainingSamples, testSamples, logStats = false)
      val aligner = new Aligner(classifier, scaler)

      val trainingResults = WholeAlgorithmMeasurer.measureFast(aligner, keepPathAndSamples(trainingTestCases, trainingSamples)).aggregate
      val testResults = WholeAlgorithmMeasurer.measureFast(aligner, keepPathAndSamples(testTestCases, testSamples)).aggregate
      logger.info(s"Finished size: $size")
      size -> ((trainingResults.raw.f1, testResults.raw.f1)) //we don't measure the joint algo just the nn
    }).seq

    learningCurve
  }
  private def toCoords(data: Seq[(Int, (Double, Double))]) = {
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
    val learningCurves = (1 to 3).map { run =>
      logger.info(s"Doing run : $run")
      oneRandomCurve(random, samples, testCases)
    }

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
    val avgCoords = toCoords(avgLearningCurve.toSeq.sortBy(_._1)) //for stability
    write.arff(avgCoords, "avg_learning_curve.arff")

    val duration = Duration.between(start, Instant.now())
    logger.info(s"Took: ${duration.toMinutes} minutes, ${duration.toMillis/1000 - duration.toMinutes * 60} seconds")
  }
}