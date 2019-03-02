package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.Pass1Aligner._
import com.kristofszilagyi.sedito.aligner.Pass1MetricCalculator.Pass1Features
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common._
import org.log4s.getLogger
import smile.classification.SoftClassifier
import smile.feature.Scaler

final case class Pass1Result(left: Selection, right: Selection, probability: Double)
object Pass1Aligner {
  private val logger = getLogger

  private def resolveWithMostProbable(results: Map[Selection, Traversable[Pass1Result]]): Traversable[Pass1Result] = {
    val mostProbables = results.map { case (_, conflictings) =>
      @SuppressWarnings(Array(Warts.TraversableOps))
      val best = conflictings.toSeq.maxBy(_.probability) //This should never fail because the map should never have empty list on the left side
      best
    }
    mostProbables
  }

  private def toResult(metrics: Features, p: Double) = {
    Pass1Result(metrics.leftWord, metrics.rightWord, p)
  }

  def alignFast(potentialMatches: Traversable[Pass1Result], log: Boolean): UnambiguousWordAlignment = {
    if (log) logger.info(s"Potentials: ${potentialMatches.size}")
    val leftResolved = resolveWithMostProbable(potentialMatches.groupBy(_.left))
    val bothResolved = resolveWithMostProbable(leftResolved.groupBy(_.right))
    if (log) logger.info(s"BothResolved: ${bothResolved.size}")
    UnambiguousWordAlignment(bothResolved.map(p => WordMatch(p.left, p.right)(Some(p.probability))).toSet)
  }

}

final class Pass1Aligner(classifier: SoftClassifier[Array[Double]], scaler: Scaler) {
  def align(left: FullText, right: FullText): UnambiguousWordAlignment = {
    val metrics = Pass1MetricCalculator.calcAlignerFeatures(left, right)
    logger.info(s"Number of metrics: ${metrics.size}")
    alignFast(findPotentialMatches(metrics), log = true)
  }

  def measureProbability(metrics: Features): Pass1Result = {
    val x = metrics.doubles
    val probs = new Array[Double](1)
    val scaledX = scaler.transform(x)
    discard(classifier.predict(scaledX, probs))
    val p = 1 - probs(0) //probs(0) is the probability of the 0 label
    toResult(metrics, p)
  }

  @SuppressWarnings(Array(Warts.DefaultArguments))
  def findPotentialMatches(metrics: Traversable[Pass1Features], minP: Double = 0.5): Traversable[Pass1Result] = {
    val probabilitiesWithMetrics = metrics.flatMap { m =>
      val result = measureProbability(m)
      if(result.probability >= minP) { //todo this could be fine tuned for optimal results
        Some(result)
      } else None
    }
    probabilitiesWithMetrics
  }

}