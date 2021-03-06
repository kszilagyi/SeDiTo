package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.IndentationCalculator._
import com.kristofszilagyi.sedito.aligner.Pass1FeatureCalculator.ArrayHolder
import com.kristofszilagyi.sedito.common.{LineIdx, Warts}

object IndentationOneSide {
  def columnNames: List[String] = {
    ((-7 to 7) map {i => s"indent$i"}).toList ++ List("lineIdx", "lineIdxFromEnd")
  }
}
final case class IndentationOneSide(indents: Seq[Int], lineIdx: LineIdx, lineIdxFromEnd: Int) {
  def addTo(array: ArrayHolder): Unit = {
    indents.foreach(i => array.add(i.toDouble))
    array.add(lineIdx.i.toDouble)
    array.add(lineIdxFromEnd.toDouble)
  }
}
final case class Indentation(left: IndentationOneSide, right: IndentationOneSide) {
  def addTo(array: ArrayHolder): Unit = {
    left.addTo(array)
    right.addTo(array)
  }

}

object IndentationCalculator {
  @SuppressWarnings(Array(Warts.Var))
  private def extend(lines: Vector[String], startIdx: LineIdx, step: Int) = {
    var result = Seq.empty[Int]
    def add(xs: Seq[Int], x: Int) = {
      if (step > 0) xs :+ x
      else x +: xs
    }
    val range = 7
    var i = startIdx.i
    while (result.size < range) {
      i += step
      if (i < 0 || i >= lines.size) {
        result = add(result, 0)
      } else {
        val line = lines(i)
        if (!line.trim.isEmpty) {
          result = add(result, line.prefixLength(_.isSpaceChar))
        }
      }
    }
    result
  }

}
final class IndentationCalculator(leftLines: Vector[String], rightLines: Vector[String]) {
  def calcIndentation(leftIdx: LineIdx, rightIdx: LineIdx): Indentation = {
    Indentation(
      IndentationOneSide(
        (extend(leftLines, leftIdx, step = -1) :+ leftLines(leftIdx.i).prefixLength(_.isSpaceChar)) ++ extend(leftLines, leftIdx, step = 1),
        leftIdx,
        leftLines.size - leftIdx.i - 1
      ),
      IndentationOneSide(
        (extend(rightLines, rightIdx, step = -1) :+ rightLines(rightIdx.i).prefixLength(_.isSpaceChar)) ++ extend(rightLines, rightIdx, step = 1),
        rightIdx,
        rightLines.size - rightIdx.i - 1
      ),
    )
  }


}
