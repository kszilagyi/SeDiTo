package com.kristofszilagyi.sedito.aligner

import java.nio.file.Path

import com.kristofszilagyi.sedito.common.Selection

trait Features {
  def doubles: Array[Double]
  def leftWord: Selection
  def rightWord: Selection
}
trait FeaturesWithResults {
  def features: Features
  def matching: Boolean
}
trait Samples {
  def featuresWithResults: Traversable[FeaturesWithResults]
}
trait PathAndSamples {
  def path: Path
  def samples: Samples
}