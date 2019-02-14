package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.Pass1MetricCalculator.PairwiseMetrics
import com.kristofszilagyi.sedito.common.{LineIdx, Warts}

// Vector: performance is important here
final class LineAlignmentCacher(leftLines: Vector[String], rightLines: Vector[String]) {
  //there is no problem if we loose an update from the cache that's why volatile is enough and no need for atomic reference
  @SuppressWarnings(Array(Warts.Var))
  @volatile
  private var cache = Map.empty[(String, String), PairwiseMetrics]
  def calcLineMetrics(leftIdx: LineIdx, rightIdx: LineIdx): PairwiseMetrics = {
    val leftLine = leftLines(leftIdx.i)
    val rightLine = rightLines(rightIdx.i)
    cache.get((leftLine, rightLine)) match {
      case Some(metrics) =>
        metrics
      case None =>
        val metrics = Pass1MetricCalculator.calcMetrics(leftLine, rightLine, math.max(leftLine.length, rightLine.length))
        cache += ((leftLine, rightLine) -> metrics)
        metrics
    }
  }
}
