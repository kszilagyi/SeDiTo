package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.FirstPassAligner.{resolveWithMostProbable, _}
import com.kristofszilagyi.sedito.aligner.Pass1MetricCalculator.Pass1Metrics
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common._
import org.log4s.getLogger
import smile.classification.SoftClassifier
import smile.feature.Scaler

final case class PartialResult(left: Selection, right: Selection, probability: Double)
object FirstPassAligner {
  private val logger = getLogger

  private def resolveWithMostProbable(results: Map[Selection, Traversable[PartialResult]]): Traversable[PartialResult] = {
    val mostProbables = results.map { case (_, conflictings) =>
      @SuppressWarnings(Array(Warts.TraversableOps))
      val best = conflictings.toSeq.maxBy(_.probability) //This should never fail because the map should never have empty list on the left side
      best
    }
    mostProbables
  }

  private def toPartialResult(metricsAndProb: (Pass1Metrics, Double)) = {
    PartialResult(metricsAndProb._1.leftWord, metricsAndProb._1.rightWord, metricsAndProb._2)
  }

}

final class FirstPassAligner(classifier: SoftClassifier[Array[Double]], scaler: Scaler) {
  def align(left: FullText, right: FullText): UnambiguousWordAlignment = {
    val metrics = Pass1MetricCalculator.calcAlignerMetrics(left, right)
    logger.info(s"Number of metrics: ${metrics.size}")
    alignFast(findPotentialMatches(metrics), log = true)
  }

  def findPotentialMatches(metrics: Traversable[Pass1Metrics], minP: Double = 0.5): Traversable[PartialResult] = {
    logger.debug("Debug metrics: \n" + metrics.mkString("\n"))

    val probabilitiesWithMetrics = metrics.flatMap { m =>
      val x = m.doubles
      val probs = new Array[Double](1)
      val scaledX = scaler.transform(x)
      discard(classifier.predict(scaledX, probs))
      logger.debug(s"${m.leftWord.toText} - ${m.rightWord.toText}, x: ${x.mkString(", ")}, p(0): ${probs(0)}")
      val p = 1 - probs(0) //probs(0) is the probability of the 0 label
      if(p >= minP) { //todo this could be fine tuned for optimal results
        Some(m -> p)
      } else None

    }
    probabilitiesWithMetrics.map(toPartialResult)
  }

  def alignFast(potentialMatches: Traversable[PartialResult], log: Boolean): UnambiguousWordAlignment = {
    if (log) logger.info(s"Potentials: ${potentialMatches.size}")
    val leftResolved = resolveWithMostProbable(potentialMatches.groupBy(_.left))
    val bothResolved = resolveWithMostProbable(leftResolved.groupBy(_.right))
    if (log) logger.info(s"BothResolved: ${bothResolved.size}")
    UnambiguousWordAlignment(bothResolved.map(p => WordMatch(p.left, p.right)(Some(p.probability))).toSet)
  }

}