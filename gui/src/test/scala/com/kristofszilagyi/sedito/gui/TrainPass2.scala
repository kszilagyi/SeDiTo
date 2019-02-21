package com.kristofszilagyi.sedito.gui

import java.nio.file.Path

import com.kristofszilagyi.sedito.aligner._
import com.kristofszilagyi.sedito.gui.Train.trainingRatio
import com.kristofszilagyi.sedito.gui.TrainAndDiff.readDataSetAndMeasureMetrics
import org.log4s.getLogger

object TrainPass2 {
  private val logger = getLogger

  private def calcFirstPass(pathAndSamples: List[Pass1PathAndSamples]) = {
    val (firstNN, firstScaler) = Main.loadAI()
    val firstPassAligner = new Pass1Aligner(firstNN, firstScaler)
    pathAndSamples.map { case Pass1PathAndSamples(path, samples) =>
      val resultsWithTruth = samples.metricsWithResults.map { sample =>
        val results = firstPassAligner.measureProbability(sample.metrics)
         Pass1ResultWithTruth(results, sample.matching)
      }
      PathAndPass1Results(path, resultsWithTruth)
    }
  }

  final case class LineMetrics(sum: Double, avg: Double)

  /**
    * @param sameLine includes main as well
    */
  final case class LineGroup(main: Pass1ResultWithTruth, sameLine: Set[Pass1Result])
  final case class PathAndLineGroups(path: Path, group: Traversable[LineGroup])
  final case class Pass1ResultWithTruth(pass1Result: Pass1Result, shouldBeMatching: Boolean)
  final case class PathAndPass1Results(path: Path, pass1Results: Traversable[Pass1ResultWithTruth])
  final case class PathAndPass2Metrics(path: Path, pass2Metrics: Traversable[Pass2Metrics])
  final case class Pass2Metrics(main: Pass1Result, line: LineMetrics) extends Metrics {
    def doubles: Array[Double] = {
      (main.probability :: line.sum :: line.avg :: Nil).toArray
    }
  }
  final case class Pass2MetricsWithResults(metrics: Pass2Metrics, matching: Boolean) extends MetricsWithResults
  final case class Pass2Samples(metricsWithResults: Traversable[Pass2MetricsWithResults]) extends Samples
  final case class PathAndPass2Samples(path: Path, samples: Pass2Samples) extends PathAndSamples

  /**
    * @param line This is only set for performance reasons: we need it as a set here and it was already a set so we don't
    *             need to call .toSet here (this might not make a difference tbh...)
    */
  private def calcLineMetrics(line: Set[Pass1Result]) = {
    assert(line.nonEmpty)
    val ps = line.toSeq.map(_.probability)
    val wordCount = (line.map(_.left).size + line.map(_.right).size) / 2.0
    val sum = ps.sum
    val avg = sum / wordCount  //this is wordCount and not ps.size because the number of ps.size = wordCount^2
    LineMetrics(sum, avg)
  }

  private def groupOneFile(pass1Results: scala.Traversable[Pass1ResultWithTruth]) = {
    logger.info(s"Grouping: ${pass1Results.size}")
    val byLeft = pass1Results.groupBy(_.pass1Result.left.lineIdx)
    val byRight  = pass1Results.groupBy(_.pass1Result.right.lineIdx)
    pass1Results.map { mainResult =>
      val sameLine = byLeft.getOrElse(mainResult.pass1Result.left.lineIdx, Traversable.empty).toSet.intersect(
                       byRight.getOrElse(mainResult.pass1Result.right.lineIdx, Traversable.empty).toSet)
      LineGroup(mainResult, sameLine.map(_.pass1Result))
    }
  }

  def calcPass2Metrics(firstPassResultsWithPath: List[TrainPass2.PathAndPass1Results]): List[PathAndPass2Samples] = {
    val groupsByPath = firstPassResultsWithPath.map { case PathAndPass1Results(path, pass1Resutls) =>
      PathAndLineGroups(path, groupOneFile(pass1Resutls))
    }

    val samplesByPath = groupsByPath.map{ case PathAndLineGroups(path, groups) =>
      val metrics = groups.map { case LineGroup(main, others) =>
        val lineMetrics = calcLineMetrics(others)
        Pass2MetricsWithResults(Pass2Metrics(main.pass1Result, lineMetrics), main.shouldBeMatching)
      }
      PathAndPass2Samples(path, Pass2Samples(metrics))
    }
    samplesByPath
  }

  def main(args: Array[String]) {
    logger.info("Start")
    //val start = Instant.now()
    val samples = readDataSetAndMeasureMetrics()


    val firstPassResultsWithPath = calcFirstPass(samples)
    logger.info(s"First pass done")
    val samplesByPath = calcPass2Metrics(firstPassResultsWithPath)

    val (trainingSamples, testSamples) = Train1Pass.shuffle(samplesByPath).splitAt((samples.size * trainingRatio).toInt)
    logger.info("Measuring pass 2 metrics is done")
    val _ = Train.train(trainingSamples, testSamples, logStats = true, hiddenLayerSize = 10)
  }
}