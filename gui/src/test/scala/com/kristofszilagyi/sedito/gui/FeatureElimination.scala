package com.kristofszilagyi.sedito.gui

import java.time.{Duration, Instant}

import com.kristofszilagyi.sedito.aligner.MetricCalculator.Metrics
import com.kristofszilagyi.sedito.gui.TrainAndDiff.{calcNumOfAttributes, generateClassifier, readDataSetAndMeasureMetrics}
import org.log4s.getLogger

object FeatureElimination extends App{
  private val logger = getLogger

  logger.info("Start")
  val start = Instant.now()
  val samples = readDataSetAndMeasureMetrics()
  val samplesWithoutFilenames = samples.map(_._2)
  val metricsWithResults = samplesWithoutFilenames.map(_.metricsWithResults)
  val numOfAttributes = calcNumOfAttributes(metricsWithResults)
  val (nestedTraining, nestedTest) = samplesWithoutFilenames.splitAt(samplesWithoutFilenames.size / 2)

  TrainAndDiff.logBasicStats(nestedTraining, nestedTest = nestedTest)
  val results = Metrics.columnNames.zipWithIndex.map { case (metric, idx) =>
    val (_, _, trainingData) = generateClassifier(nestedTraining = nestedTraining, nestedTest = nestedTest, numOfAttributes, idxesToExclude = Set(idx))
    val f1s = trainingData.f1s
    logger.info(s"Excluded metric: $metric. F1s: $f1s")
    metric -> f1s
  }
  logger.info("Results sorted by test")
  results.sortBy(_._2.test).foreach { case (metric, f1) =>
    logger.info(s"$metric: ${f1.test}")
  }

  logger.info("Results sorted by training")
  results.sortBy(_._2.training).foreach { case (metric, f1) =>
    logger.info(s"$metric: ${f1.training}")
  }

  val duration = Duration.between(start, Instant.now())
  logger.info(s"Took: ${duration.toMinutes} minutes, ${duration.toMillis/1000 - duration.toMinutes * 60} seconds")
}
