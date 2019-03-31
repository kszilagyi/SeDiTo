package com.kristofszilagyi.sedito.common

import com.kristofszilagyi.sedito.common.IndentationCalculator._

final case class IndentationOneSide(indents: Seq[Int], lineIdx: LineIdx, lineIdxFromEnd: Int)
final case class Indentation(left: IndentationOneSide, right: IndentationOneSide)

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
