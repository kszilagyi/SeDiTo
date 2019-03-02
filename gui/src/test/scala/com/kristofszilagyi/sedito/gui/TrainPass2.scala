package com.kristofszilagyi.sedito.gui

import java.nio.file.Path

import com.kristofszilagyi.sedito.aligner.Pass1MetricCalculator.Pass1Features
import com.kristofszilagyi.sedito.aligner._
import com.kristofszilagyi.sedito.common.{LineIdx, Selection, WordMatch, Wordizer}
import com.kristofszilagyi.sedito.gui.Train.trainingRatio
import com.kristofszilagyi.sedito.gui.TrainAndDiff.readDataSetAndMeasureFeatures
import org.log4s.getLogger

object TrainPass2 {
  private val logger = getLogger

  def calcFirstPass(pathAndSamples: List[Pass1PathAndSamples]): List[PathAndPass1Results] = {
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

  final case class LineFeatures(sum: Double, avg: Double, weightedSum: Double, weightedAvg: Double, numWordsMatching: Double, avgNumberOfWordsMatching: Double) {
    def doubles: List[Double] = List(sum, avg, weightedSum, weightedAvg, numWordsMatching, avgNumberOfWordsMatching)
  }

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
      (main.probability +: mainPass1Features.doubles) ++ line.doubles
    }

    def leftWord: Selection = main.left

    def rightWord: Selection = main.right
  }
  final case class Pass2FeaturesWithResults(features: Pass2Features, matching: Boolean) extends FeaturesWithResults
  final case class Pass2Samples(featuresWithResults: Traversable[Pass2FeaturesWithResults]) extends Samples
  final case class PathAndPass2Samples(path: Path, samples: Pass2Samples) extends PathAndSamples

  private def calcLineFeaturesFromMatches(matches: Set[WordMatch], lineLength: Int) = {
    assert(lineLength > 0)
    assert(matches.nonEmpty)
    val sum = matches.map(_.probability.get).sum //.get if it's not there it's a bug!
    val weightedSum = matches.map(m => m.probability.get * (m.left.length + m.right.length)).sum
    val avg = sum / lineLength
    val weightedAvg = weightedSum / lineLength
    LineFeatures(sum, avg, weightedSum, weightedAvg, matches.size.toDouble, matches.size.toDouble / lineLength)
  }

  private def calcLineFeaturesFromResult(result: TrainPass2.Pass1ResultWithTruth,
                                         alignmentByLeft: Map[LineIdx, Set[WordMatch]],
                                         alignmentByRight: Map[LineIdx, Set[WordMatch]]) = {
    val leftMatches = alignmentByLeft.getOrElse(result.pass1Features.leftLineIdx, Set.empty)
    val rightMatches = alignmentByRight.getOrElse(result.pass1Features.rightLineIdx, Set.empty)
    val commonMatches = leftMatches.intersect(rightMatches)
    if (commonMatches.nonEmpty) {
      val firstMatch = commonMatches.head
      val leftLineLength = lineLength(firstMatch.left)
      val rightLineLength = lineLength(firstMatch.right)
      calcLineFeaturesFromMatches(commonMatches, (leftLineLength + rightLineLength) / 2) //todo investigate if an nn can approximate sum + (leftline + rightline)
    } else {
      LineFeatures(0, 0, 0, 0, 0, 0)
    }
  }

  private def lineLength(s: Selection) = {
    Wordizer.toWords(s.line).map(_.length).sum
  }

  def calcPass2Features(firstPassResultsWithPath: List[TrainPass2.PathAndPass1Results]): List[PathAndPass2Samples] = {
    firstPassResultsWithPath.map { case PathAndPass1Results(path, pass1Results) =>
      val alignment = Pass1Aligner.alignFast(pass1Results.map(_.pass1Result), log = false)
      val alignmentByLeft = alignment.matches.groupBy(_.left.lineIdx)
      val alignmentByRight = alignment.matches.groupBy(_.right.lineIdx)
      val pass2FeaturesWithResults = pass1Results.map { result =>
        val lineFeatures = calcLineFeaturesFromResult(result, alignmentByLeft, alignmentByRight)
        Pass2FeaturesWithResults(
          Pass2Features(result.pass1Result, result.pass1Features, lineFeatures),
          matching = result.shouldBeMatching
        )
      }
      PathAndPass2Samples(path, Pass2Samples(pass2FeaturesWithResults))
    }
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