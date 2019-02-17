package com.kristofszilagyi.sedito.aligner

import java.nio.file.Path

trait Metrics {
  def doubles: Array[Double]
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