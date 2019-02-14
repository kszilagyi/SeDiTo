package com.kristofszilagyi.sedito.gui

import java.awt.Color

import smile.{plot, read}
//ideas going forward:
//review good_cand_for_char_based_compare -> expected is wrong
//block edge detection based on spaces
//group lines together, calculate metrics for first phase and do an other run -> this should help with the typical problem of single
// characters going to a different line
// look at the data and see what's wrong
// more data
// add made up test cases for, rename while the base is the same but otherwise quite different: x -> x_base for ezxample
// and capitalisation like: myBaby -> MY_BABY

// textblocklinked1to1_cpp -> feedback probably would help
// grid_py -> feedback would help
// test_model_py2 -> feedback would help a lot
//
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
