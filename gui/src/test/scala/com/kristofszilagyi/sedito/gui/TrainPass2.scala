package com.kristofszilagyi.sedito.gui

import java.nio.file.Path

import com.kristofszilagyi.sedito.aligner.Pass1MetricCalculator.Pass1Features
import com.kristofszilagyi.sedito.aligner._
import com.kristofszilagyi.sedito.common.Selection
import com.kristofszilagyi.sedito.gui.Train.trainingRatio
import com.kristofszilagyi.sedito.gui.TrainAndDiff.readDataSetAndMeasureFeatures
import org.log4s.getLogger

object TrainPass2 {
  private val logger = getLogger

  private def calcFirstPass(pathAndSamples: List[Pass1PathAndSamples]) = {
    val (firstNN, firstScaler) = Main.loadAI()
    val firstPassAligner = new Pass1Aligner(firstNN, firstScaler)
    pathAndSamples.map { case Pass1PathAndSamples(path, samples) =>
      val resultsWithTruth = samples.featuresWithResults.map { sample =>
        val results = firstPassAligner.measureProbability(sample.features)
         Pass1ResultWithTruth(results, sample.features, sample.matching)
      }
      PathAndPass1Results(path, resultsWithTruth)
    }
  }

  final case class LineFeatures(sum: Double, avg: Double)

  /**
    * @param sameLine includes main as well
    */
  final case class LineGroup(main: Pass1ResultWithTruth, sameLine: Set[Pass1Result])
  final case class PathAndLineGroups(path: Path, group: Traversable[LineGroup])
  final case class Pass1ResultWithTruth(pass1Result: Pass1Result, pass1Features: Pass1Features, shouldBeMatching: Boolean)
  final case class PathAndPass1Results(path: Path, pass1Results: Traversable[Pass1ResultWithTruth])
  final case class PathAndPass2Features(path: Path, pass2Features: Traversable[Pass2Features])
  final case class Pass2Features(main: Pass1Result, mainPass1Features: Pass1Features, line: LineFeatures) extends Features {
    def doubles: Array[Double] = {
      mainPass1Features.doubles ++ (main.probability :: line.sum :: line.avg :: Nil)
    }

    def leftWord: Selection = main.left

    def rightWord: Selection = main.right
  }
  final case class Pass2FeaturesWithResults(features: Pass2Features, matching: Boolean) extends FeaturesWithResults
  final case class Pass2Samples(featuresWithResults: Traversable[Pass2FeaturesWithResults]) extends Samples
  final case class PathAndPass2Samples(path: Path, samples: Pass2Samples) extends PathAndSamples

  /**
    * @param line This is only set for performance reasons: we need it as a set here and it was already a set so we don't
    *             need to call .toSet here (this might not make a difference tbh...)
    */
  private def calcLineFeatures(line: Set[Pass1Result]) = {
    assert(line.nonEmpty)
    val ps = line.toSeq.map(_.probability)
    val wordCount = (line.map(_.left).size + line.map(_.right).size) / 2.0 //this make sense because they are sets
    val sum = ps.sum
    val avg = sum / wordCount  //this is wordCount and not ps.size because the number of ps.size = wordCount^2
    LineFeatures(sum, avg)
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

  def calcPass2Features(firstPassResultsWithPath: List[TrainPass2.PathAndPass1Results]): List[PathAndPass2Samples] = {
    val groupsByPath = firstPassResultsWithPath.map { case PathAndPass1Results(path, pass1Resutls) =>
      PathAndLineGroups(path, groupOneFile(pass1Resutls))
    }

    val samplesByPath = groupsByPath.map{ case PathAndLineGroups(path, groups) =>
      val metrics = groups.map { case LineGroup(main, sameLine) =>
        val lineMetrics = calcLineFeatures(sameLine)
        Pass2FeaturesWithResults(Pass2Features(main.pass1Result, main.pass1Features, lineMetrics), main.shouldBeMatching)
      }
      PathAndPass2Samples(path, Pass2Samples(metrics))
    }
    samplesByPath
  }

  def main(args: Array[String]) {
    logger.info("Start")
    //val start = Instant.now()
    val samples = readDataSetAndMeasureFeatures()


    val firstPassResultsWithPath = calcFirstPass(samples)
    logger.info(s"First pass done")
    val samplesByPath = calcPass2Features(firstPassResultsWithPath)

    val (trainingSamples, testSamples) = Train1Pass.shuffle(samplesByPath).splitAt((samples.size * trainingRatio).toInt)
    logger.info("Measuring pass 2 metrics is done")
    val _ = Train.train(trainingSamples, testSamples, logStats = true, hiddenLayerSize = 50)
  }
}