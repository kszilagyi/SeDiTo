package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.Aligner.{findPotentialMatches, resolveWithMostProbable}
import com.kristofszilagyi.sedito.aligner.MetricCalculator.Metrics
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common._
import org.log4s.getLogger
import smile.classification.SoftClassifier
import smile.feature.Scaler
import Aligner._

final case class PartialResult(left: Selection, right: Selection, probability: Double)
object Aligner {
  private val logger = getLogger

  private def resolveWithMostProbable(results: Map[Selection, Traversable[PartialResult]]): Traversable[PartialResult] = {
    val mostProbables = results.map { case (_, conflictings) =>
      @SuppressWarnings(Array(Warts.TraversableOps))
      val best = conflictings.toSeq.maxBy(_.probability) //This should never fail because the map should never have empty list on the left side
      best
    }
    mostProbables
  }

  private def toPartialResult(metricsAndProb: (Metrics, Double)) = {
    PartialResult(metricsAndProb._1.leftWord, metricsAndProb._1.rightWord, metricsAndProb._2)
  }

  private def findPotentialMatches(classifier: SoftClassifier[Array[Double]], scaler: Scaler, metrics: Traversable[Metrics]): Traversable[PartialResult] = {
    logger.debug("Debug metrics: \n" + metrics.mkString("\n"))

    val probabilitiesWithMetrics = metrics.flatMap { m =>
      val x = m.doubles
      val probs = new Array[Double](1)
      val scaledX = scaler.transform(x)
      val prediction = classifier.predict(scaledX, probs)
      logger.debug(s"${m.leftWord.toText} - ${m.rightWord.toText}, x: ${x.mkString(", ")}, p(0): ${probs(0)}")
      if(prediction ==== 1) { //todo this could be fine tuned for optimal results
        val p = 1 - probs(0) //probs(0) is the probability of the 0 label
        assert(p >= 0.5, s"p = $p")
        Some(m -> p)
      } else None

    }
    probabilitiesWithMetrics.map(toPartialResult)
  }
}

final class Aligner(classifier: SoftClassifier[Array[Double]], scaler: Scaler) {
  def align(left: FullText, right: FullText): UnambiguousWordAlignment = {
    val metrics = MetricCalculator.calcAlignerMetrics(left, right)
    logger.info(s"Number of metrics: ${metrics.size}")
    alignFast(metrics, log = true)
  }

  def alignFast(metrics: Traversable[Metrics], log: Boolean): UnambiguousWordAlignment = {
    val potentialMatches = findPotentialMatches(classifier, scaler, metrics)
    if (log) logger.info(s"Potentials: ${potentialMatches.size}")
    val leftResolved = resolveWithMostProbable(potentialMatches.groupBy(_.left))
    val bothResolved = resolveWithMostProbable(leftResolved.groupBy(_.right))
    if (log) logger.info(s"BothResolved: ${bothResolved.size}")
    UnambiguousWordAlignment(bothResolved.map(p => WordMatch(p.left, p.right)(Some(p.probability))).toSet)
  }

}