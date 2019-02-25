package com.kristofszilagyi.sedito.gui

import java.nio.file.{Files, Paths}

import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.gui.Train.trainingRatio
import com.kristofszilagyi.sedito.gui.TrainAndDiff.readDataSetAndMeasureMetrics
import org.log4s.getLogger
import smile.feature.Scaler
import smile.write

import scala.collection.JavaConverters._

object TrainIterative {
  private val logger = getLogger
//
//
//  private def calcFirstPass(pathAndSamples: List[Pass1PathAndSamples], nn: SoftClassifier[Array[Double]], scaler: Scaler) = {
//
//    val firstPassAligner = new Pass1Aligner(nn, scaler)
//    pathAndSamples.map { case Pass1PathAndSamples(path, samples) =>
//      val resultsWithTruth = samples.metricsWithResults.map { sample =>
//        val results = firstPassAligner.measureProbability(sample.metrics)
//         Pass1ResultWithTruth(results, sample.metrics, sample.matching)
//      }
//      PathAndPass1Results(path, resultsWithTruth)
//    }
//  }
//
//  private def calcOtherPass(pathAndSamples: List[PathAndIterativeSamples], nn: SoftClassifier[Array[Double]], scaler: Scaler) = {
//
//    val firstPassAligner = new Pass1Aligner(nn, scaler)
//    pathAndSamples.map { case PathAndIterativeSamples(path, samples) =>
//      val resultsWithTruth = samples.metricsWithResults.map { sample =>
//        val results = firstPassAligner.measureProbability(sample.metrics)
//        Pass1ResultWithTruth(results, sample.metrics, sample.matching)
//      }
//      PathAndPass1Results(path, resultsWithTruth)
//    }
//  }
//
//
//  final case class Pass1ResultWithTruth(pass1Result: Pass1Result, pass1Metrics: Pass1Metrics, shouldBeMatching: Boolean)
//  final case class PathAndPass1Results(path: Path, pass1Results: Traversable[Pass1ResultWithTruth])
//  final case class IterativeMetrics(main: Pass1Result, mainPass1Metrics: Pass1Metrics) extends Metrics {
//    def doubles: Array[Double] = {
//      mainPass1Metrics.doubles :+ main.probability
//    }
//  }
//  final case class IterativeMetricsWithResults(metrics: IterativeMetrics, matching: Boolean) extends MetricsWithResults
//  final case class IterativeSamples(metricsWithResults: Traversable[IterativeMetricsWithResults]) extends Samples
//  final case class PathAndIterativeSamples(path: Path, samples: IterativeSamples) extends PathAndSamples
//
//  //if we don't filter the results are not better...
//  def calcIterativeMetrics(firstPassResultsWithPath: List[TrainIterative.PathAndPass1Results], filterP: Double): List[PathAndIterativeSamples] = {
//
//    firstPassResultsWithPath.map { pathAndRes =>
//      val samples = IterativeSamples(pathAndRes.pass1Results.filter(_.pass1Result.probability > filterP).map { resultWithTruth =>
//        val metrics = IterativeMetrics(resultWithTruth.pass1Result, resultWithTruth.pass1Metrics)
//        IterativeMetricsWithResults(metrics, resultWithTruth.shouldBeMatching)
//      })
//      PathAndIterativeSamples(pathAndRes.path, samples)
//    }
//  }

//  private def onePass() = {
//
//  }

  def main(args: Array[String]) {
    logger.info("Start")
    //val start = Instant.now()
    val samples = readDataSetAndMeasureMetrics()

    val (_, firstScaler) = Main.loadAI()
    val (trainingSamples, testSamples) = Train1Pass.shuffle(samples).splitAt((samples.size * trainingRatio).toInt)
//    val trainingResult = calcFirstPass(trainingSamples, firstNN, firstScaler)
//    val testResult = calcFirstPass(testSamples, firstNN, firstScaler)
//    logger.info(s"First pass done")
//    val trainingMetrics = calcIterativeMetrics(trainingResult, filterP = 0.1)
//    val testMetrics = calcIterativeMetrics(testResult, filterP = 0.1)
//
//    val _ = Train.train(trainingMetrics, testMetrics, logStats = true, hiddenLayerSize = 50)
  }
  // see if an nn only on the pass1 result can be at least as good
}