package com.kristofszilagyi.sedito.aligner

import java.nio.file.Path

import com.kristofszilagyi.sedito.common.Selection

trait Metrics {
  def doubles: Array[Double]
  def leftWord: Selection
  def rightWord: Selection
}
trait MetricsWithResults {
  def metrics: Metrics
  def matching: Boolean
}
trait Samples {
  def metricsWithResults: Traversable[MetricsWithResults]
}
trait PathAndSamples {
  def path: Path
  def samples: Samples
}