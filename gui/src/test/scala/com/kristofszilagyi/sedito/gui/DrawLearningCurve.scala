package com.kristofszilagyi.sedito.gui

import java.awt.Color

import smile.{plot, read}

object Draw {
  def draw(filename: String) {
    val data = read.arff(filename, 2)
    val x = data.x()
    val labels = data.labels()
    val _ = plot.plot(data = x, label = labels,
      palette = Array(Color.BLUE, Color.RED), legend = Array('x', 'x'))
  }
}

object DrawFlattenedLearningCurve {
  def main(args: Array[String]): Unit = {
    Draw.draw("flattened_learning_curve.arff")
  }
}

object DrawAvgLearningCurve {
  def main(args: Array[String]): Unit = {
    Draw.draw("avg_learning_curve.arff")

  }
}
