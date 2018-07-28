package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common.{LineIdx, LineMatch}

final case class LineRange(from: LineIdx, to: LineIdx) {
  def positive: Boolean = to.i - from.i > 0
  def without(line: LineIdx): Seq[LineRange] = {
    if (line < from || line >= to) Seq(this)
    else Seq(LineRange(from, line), LineRange(line + 1, to)).filter(_.positive)
  }

  def overlap(other: LineRange): Boolean = {
    (from < other.from && to > other.from) ||
      (from < other.to && to > other.from)
  }
}
object EquivalencePoint {
  def from(left: (Int, Int), right: (Int, Int)) =
    EquivalencePoint(LineRange(LineIdx(left._1), LineIdx(left._2)), LineRange(LineIdx(right._1), LineIdx(right._2)))
}

/**
  * If there is a zero length range that means that the insertion is just before the position
  *
  *
  */
final case class EquivalencePoint(left: LineRange, right: LineRange) {
  def positive: Boolean = left.positive || right.positive
  def withoutRight(line: LineIdx): Seq[EquivalencePoint] = {
    right.without(line).map(EquivalencePoint(left, _))
  }
  def withoutLeft(line: LineIdx): Seq[EquivalencePoint] = {
    left.without(line).map(EquivalencePoint(_, right))
  }
}


object InsertionPointCalculator {

  def calc(notMoved: Traversable[LineMatch], moved: Traversable[LineMatch]): Traversable[EquivalencePoint] = {
    // sorting by left imply sorting by right
    val notMovedSorted = notMoved.toList.sortBy(_.leftLineIdx)
    @SuppressWarnings(Array(Var))
    var last = LineMatch.create(-1, -1)
    val builder = Seq.newBuilder[EquivalencePoint]
    // this could be done with sliding though I am not convinced it would be any better
    notMovedSorted.foreach { current =>
      val eq = EquivalencePoint(LineRange(last.leftLineIdx + 1, current.leftLineIdx), LineRange(last.rightLineIdx + 1, current.rightLineIdx))
      if (eq.positive)
        discard(builder += eq)
      last = current
    }
    val eqWoMoves = builder.result()
    moved.foldLeft(eqWoMoves) { case (eqs, movedLine) =>
      eqs.flatMap(_.withoutRight(movedLine.rightLineIdx).flatMap(_.withoutLeft(movedLine.leftLineIdx)))
    }


  }
}
