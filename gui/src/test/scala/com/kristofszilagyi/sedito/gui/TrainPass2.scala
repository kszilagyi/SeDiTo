package com.kristofszilagyi.sedito.gui

import java.nio.file.Path

import com.kristofszilagyi.sedito.aligner.Pass1MetricCalculator.Pass1Features
import com.kristofszilagyi.sedito.aligner._
import com.kristofszilagyi.sedito.common.AssertionEx._
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common._
import com.kristofszilagyi.sedito.gui.Train.trainingRatio
import com.kristofszilagyi.sedito.gui.TrainAndDiff.readDataSetAndMeasureFeatures
import org.log4s.getLogger

import scala.collection.Searching.{Found, search}
import scala.math._

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

  final class LineFeatures(sum: Double, avg: Double, weightedSum: Double, weightedAvg: Double, numWordsMatching: Double, avgNumberOfWordsMatching: Double) {
    def doubles: List[Double] = List(sum, avg, weightedSum, weightedAvg, numWordsMatching, avgNumberOfWordsMatching)
  }

  final class SingleContextFeatures(matchCount: Int) {
    //todo correct for edges (when the matchCount can't be high enough)
    def doubles: List[Double] = List(matchCount.toDouble)
  }

  final class AllContextFeatures(all: List[SingleContextFeatures]) {
    def doubles: List[Double] = all.flatMap(_.doubles)
  }

  /**
    * @param sameLine includes main as well
    */
  final case class LineGroup(main: Pass1ResultWithTruth, sameLine: Set[Pass1Result])
  final case class PathAndLineGroups(path: Path, group: Traversable[LineGroup])
  final case class Pass1ResultWithTruth(pass1Result: Pass1Result, pass1Features: Pass1Features, shouldBeMatching: Boolean)
  final case class PathAndPass1Results(path: Path, pass1Results: Traversable[Pass1ResultWithTruth])
  final case class PathAndPass2Features(path: Path, pass2Features: Traversable[Pass2Features])
  final case class Pass2Features(main: Pass1Result, mainPass1Features: Pass1Features, line: LineFeatures, contextFeatures: AllContextFeatures) extends Features {
    def doubles: Array[Double] = {
      (main.probability +: mainPass1Features.doubles) ++ line.doubles ++ contextFeatures.doubles
    }

    def leftWord: Selection = main.left

    def rightWord: Selection = main.right
  }
  final case class Pass2FeaturesWithResults(features: Pass2Features, matching: Boolean) extends FeaturesWithResults
  final case class Pass2Samples(featuresWithResults: Traversable[Pass2FeaturesWithResults]) extends Samples
  final case class PathAndPass2Samples(path: Path, samples: Pass2Samples) extends PathAndSamples

  @SuppressWarnings(Array(Warts.OptionPartial))
  private def calcLineFeaturesFromMatches(matches: Set[WordMatch], lineLength: Int) = {
    assert(lineLength > 0)
    assert(matches.nonEmpty)
    val sum = matches.map(_.probability.get).sum //.get if it's not there it's a bug!
    val weightedSum = matches.map(m => m.probability.get * (m.left.length + m.right.length)).sum
    val avg = sum / lineLength
    val weightedAvg = weightedSum / lineLength
    new LineFeatures(sum, avg, weightedSum, weightedAvg, matches.size.toDouble, matches.size.toDouble / lineLength)
  }

  private def calcLineFeaturesFromResult(result: TrainPass2.Pass1ResultWithTruth,
                                         matchesByLeft: Map[LineIdx, Set[WordMatch]],
                                         matchesByRight: Map[LineIdx, Set[WordMatch]]) = {
    val leftMatches = matchesByLeft.getOrElse(result.pass1Features.leftLineIdx, Set.empty)
    val rightMatches = matchesByRight.getOrElse(result.pass1Features.rightLineIdx, Set.empty)
    val commonMatches = leftMatches.intersect(rightMatches)
    if (commonMatches.nonEmpty) {
      val firstMatch = commonMatches.head
      val leftLineLength = lineLength(firstMatch.left)
      val rightLineLength = lineLength(firstMatch.right)
      calcLineFeaturesFromMatches(commonMatches, (leftLineLength + rightLineLength) / 2) //todo investigate if an nn can approximate sum + (leftline + rightline)
    } else {
      new LineFeatures(0, 0, 0, 0, 0, 0)
    }
  }

  private def withinContextSize(center: WordMatch, contextSize: Int, idx: Int, leftSortedMatches: Vector[WordMatch], absoluteFrom: WordMatch => Int): Boolean = {
    absoluteFrom(center) - abs(contextSize) < absoluteFrom(leftSortedMatches(idx)) && absoluteFrom(leftSortedMatches(idx)) < absoluteFrom(center) + abs(contextSize)
  }

  // we only need one version of this (leftSortedMatches and not rightSortedMatches) because both end of a match has to be within
  // contextSize. So if we find all when it is within contextSize for left than we implicitly we all for right
  private def extendContextToDirection(center: WordMatch, leftSortedMatches: Vector[WordMatch], startIdx: Int, step: Int, contextSize: Int) = {
    val builder = Vector.newBuilder[WordMatch]
    @SuppressWarnings(Array(Warts.Var))
    var idx = startIdx + step
    while (0 <= idx && idx < leftSortedMatches.size &&
        withinContextSize(center, contextSize, idx, leftSortedMatches, _.left.absoluteFrom)) {

      if (withinContextSize(center, contextSize, idx, leftSortedMatches, _.right.absoluteFrom)) {
        discard(builder += leftSortedMatches(idx))
      }
      idx += step
    }
    builder.result()
  }

  private def context(center: WordMatch, leftSortedMatches: Vector[WordMatch], ordering: Ordering[WordMatch], contextSize: Int) = {
    leftSortedMatches.search(center)(ordering) match {
      case Found(idx) =>
        val extendLeft = extendContextToDirection(center, leftSortedMatches, idx, step = -1, contextSize = contextSize)
        val extendRight = extendContextToDirection(center, leftSortedMatches, idx, step = 1, contextSize = contextSize)
        extendLeft ++ extendRight
      case _ => fail(s"Bug: $center couldn't be found")
    }
  }

  private def calcContextFeature(context: Vector[WordMatch]) = {
    new SingleContextFeatures(context.size)
  }

  private def calcContextFeatures(center: WordMatch, alignment: UnambiguousWordAlignment): AllContextFeatures = {
    val leftSortedMatch = alignment.matches.toVector.sortBy(_.left.absoluteFrom)
    val featureForSizes = List(1, 2, 4, 8, 16, 32, 64, 128, 256) map { contextSize =>
      calcContextFeature(context(center, leftSortedMatch, Ordering.by(_.left.absoluteFrom), contextSize))
    }
    new AllContextFeatures(featureForSizes)
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
        val contextFeatures = calcContextFeatures(result.pass1Result.toMatch, alignment)
        Pass2FeaturesWithResults(
          Pass2Features(result.pass1Result, result.pass1Features, lineFeatures, contextFeatures),
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