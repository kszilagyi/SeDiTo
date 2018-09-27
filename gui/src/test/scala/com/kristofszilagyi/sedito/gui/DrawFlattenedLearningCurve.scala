package com.kristofszilagyi.sedito.gui

import java.awt.Color

import smile.{plot, read}


object DrawFlattenedLearningCurve {
  def main(args: Array[String]): Unit = {
    val data = read.arff("flattened_learning_curve.arff", 2)
    val x = data.x()
    val labels = data.labels()
    val _ = plot.plot(data = x, label = labels,
      palette = Array(Color.BLUE, Color.RED), legend = Array('x', 'x'))
  }


}
