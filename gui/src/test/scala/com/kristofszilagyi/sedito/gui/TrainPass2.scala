package com.kristofszilagyi.sedito.gui

import java.nio.file.Path

import com.kristofszilagyi.sedito.aligner._
import com.kristofszilagyi.sedito.gui.Train.trainingRatio
import com.kristofszilagyi.sedito.gui.TrainAndDiff.readDataSetAndMeasureMetrics
import org.log4s.getLogger
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._

import scala.util.Random

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

  private final case class LineMetrics(sum: Double, avg: Double)
  private final case class LineGroup(main: Pass1ResultWithTruth, others: Traversable[Pass1Result])
  private final case class PathAndLineGroups(path: Path, group: Traversable[LineGroup])
  private final case class Pass1ResultWithTruth(pass1Result: Pass1Result, shouldBeMatching: Boolean)
  private final case class PathAndPass1Results(path: Path, pass1Results: Traversable[Pass1ResultWithTruth])
  private final case class PathAndPass2Metrics(path: Path, pass2Metrics: Traversable[Pass2Metrics])
  private final case class Pass2Metrics(main: Pass1Result, line: LineMetrics) extends Metrics {
    def doubles: Array[Double] = {
      (main.probability :: line.sum :: line.avg :: Nil).toArray
    }
  }
  private final case class Pass2MetricsWithResults(metrics: Pass2Metrics, matching: Boolean) extends MetricsWithResults
  private final case class Pass2Samples(metricsWithResults: Traversable[Pass2MetricsWithResults]) extends Samples
  private final case class PathAndPass2Samples(path: Path, samples: Pass2Samples) extends PathAndSamples

  private def calcLineMetrics(line: Traversable[Pass1Result]) = {
    assert(line.nonEmpty)
    val ps = line.map(_.probability)
    val wordCount = line.map(_.left).toSet.size + line.map(_.right).toSet.size
    val sum = ps.sum
    val avg = sum / wordCount  //this is wordCount and not ps.size because the number of ps.size = wordCount^2
    LineMetrics(sum, avg)
  }

  private def sameLineButNotTheSame(main: Pass1Result, other: Pass1Result): Boolean = {
    main != other && (main.left ==== other.left && main.right ==== other.right)
  }

  private def groupOneFile(pass1Results: scala.Traversable[Pass1ResultWithTruth]) = {
    pass1Results.map { singleResult =>
      val related = pass1Results.filter(other => sameLineButNotTheSame(singleResult.pass1Result, other.pass1Result))
      LineGroup(singleResult, related.map(_.pass1Result))
    }
  }

  def main(args: Array[String]) {
    logger.info("Start")
    //val start = Instant.now()
    val samples = readDataSetAndMeasureMetrics()


    val firstPassResultsWithPath = calcFirstPass(samples)
    logger.info(s"whole size: ${firstPassResultsWithPath.size}")

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

    val random = new Random(124) //make it repeatable but avoid weird dependence on file structure
    val (trainingSamples, testSamples) = random.shuffle(samplesByPath).splitAt((samples.size * trainingRatio).toInt)
    val _ = Train.train(trainingSamples, testSamples, logStats = true, hiddenLayerSize = 10)
  }
}
