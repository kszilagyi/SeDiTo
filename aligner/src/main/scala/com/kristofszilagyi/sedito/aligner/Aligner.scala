package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.Aligner.{findPotentialMatches, resolveWithMostProbable}
import com.kristofszilagyi.sedito.aligner.MetricCalculator.Metrics
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common._
import org.log4s.getLogger
import smile.classification.SoftClassifier
import smile.feature.Scaler

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

  private def findPotentialMatches(classifier: SoftClassifier[Array[Double]], scaler: Scaler, metrics: IndexedSeq[Metrics]): Traversable[PartialResult] = {
    logger.debug("Debug metrics: \n" + metrics.mkString("\n"))
    val xs = metrics.map(m => m -> m.doubles).toArray

    val probabilitiesWithMetrics = xs.flatMap { case (m, x) =>
      val probs = new Array[Double](1)
      val scaledX = scaler.transform(x)
      val prediction = classifier.predict(scaledX, probs)
      logger.debug(s"${m.leftWord.toText} - ${m.rightWord.toText}, x: ${x.mkString(", ")}, p(0): ${probs(0)}")
      if(prediction ==== 1) {
        val p = 1 - probs(0) //probs(0) is the probability of the 0 label
        assert(p >= 0.5, s"p = $p")
        Some(m -> p).toList
      } else None.toList

    }
    probabilitiesWithMetrics.map(toPartialResult).toTraversable
  }
}

final class Aligner(classifier: SoftClassifier[Array[Double]], scaler: Scaler) {
  def align(left: FullText, right: FullText): UnambiguousWordAlignment = {
    val metrics = MetricCalculator.calcAlignerMetrics(left, right)
    alignFast(metrics)
  }

  def alignFast(metrics: IndexedSeq[Metrics]): UnambiguousWordAlignment = {
    val potentialMatches = findPotentialMatches(classifier, scaler, metrics)
    val leftResolved = resolveWithMostProbable(potentialMatches.groupBy(_.left))
    val bothResolved = resolveWithMostProbable(leftResolved.groupBy(_.right))
    UnambiguousWordAlignment(bothResolved.map(p => WordMatch(p.left, p.right)).toSet)
  }
}
