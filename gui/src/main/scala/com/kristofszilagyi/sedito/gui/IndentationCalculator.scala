package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.LineIdx


final case class IndentationOneSide(before: Seq[Int], current: Int, after: Seq[Int], lineIdx: LineIdx, lineIdxFromEnd: Int)
final case class Indentation(left: IndentationOneSide, right: IndentationOneSide)

final class IndentationCalculator(leftLines: Vector[String], rightLines: Vector[String]) {
  def calcIndentation(leftIdx: LineIdx, rightIdx: LineIdx): Indentation = {
    Indentation(
      IndentationOneSide(Seq(0, 0, 0, 0, 0, 0, 0), 0, Seq(0, 0, 0, 0, 0, 0, 0), leftIdx, leftLines.size - leftIdx.i - 1),
      IndentationOneSide(Seq(0, 0, 0, 0, 0, 0, 0), 0, Seq(0, 0, 0, 0, 0, 0, 0), rightIdx, rightLines.size - rightIdx.i - 1),
    )
  }


}
