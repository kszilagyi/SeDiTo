package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.Aligner.{findPotentialMatches, resolveWithMostProbable}
import com.kristofszilagyi.sedito.aligner.MetricCalculator.Metrics
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.{Selection, UnambiguousWordAlignment, WordMatch}
import org.log4s.getLogger
import smile.classification.LogisticRegression

final case class PartialResult(left: Selection, right: Selection, probability: Double)
object Aligner {
  private val logger = getLogger

  private def resolveWithMostProbable(results: Map[Selection, Traversable[PartialResult]]): Traversable[PartialResult] = {
    val mostProbables = results.map { case (_, conflictings) =>
      val best = conflictings.toSeq.maxBy(_.probability) //This should never fail because the map should never have empty list on the left side
      best
    }
    mostProbables
  }

  private def toPartialResult(metricsAndProb: (Metrics, Double)) = {
    PartialResult(metricsAndProb._1.leftWord, metricsAndProb._1.rightWord, metricsAndProb._2)
  }

  private def findPotentialMatches(logit: LogisticRegression, left: String, right: String): Traversable[PartialResult] = {
    val metrics = MetricCalculator.calcAlignerMetrics(left, right)
    logger.debug("Debug metrics: \n" + metrics.mkString("\n"))
    val xs = metrics.map(m => m -> m.toLdLenSimDouble).toArray

    val probabilitiesWithMetrics = xs.flatMap { case (m, x) =>
      val probs = new Array[Double](2)
      val prediction = logit.predict(x, probs)
      if(prediction ==== 1) {
        val p = probs(1)
        assert(p >= 0.5, s"p = $p")
        Some(m -> p).toList
      } else None.toList

    }
    probabilitiesWithMetrics.map(toPartialResult).toTraversable
  }
}

final class Aligner(logit: LogisticRegression) {
  def align(left: String, right: String): UnambiguousWordAlignment = {
    val potentialMatches = findPotentialMatches(logit, left, right)
    val leftResolved = resolveWithMostProbable(potentialMatches.groupBy(_.left))
    val bothResolved = resolveWithMostProbable(leftResolved.groupBy(_.right))
    UnambiguousWordAlignment(bothResolved.map(p => WordMatch(p.left, p.right)).toSet)
  }
}
