package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common.{LineIdx, LineMatch}

object LineRange {
  def empty: LineRange = LineRange(LineIdx(0), LineIdx(0))
}
final case class LineRange(from: LineIdx, to: LineIdx) {
  def positive: Boolean = size > 0
  def without(line: LineIdx): Seq[LineRange] = {
    if (line < from || line >= to) Seq(this)
    else Seq(LineRange(from, line), LineRange(line + 1, to)).filter(_.positive)
  }

  def overlap(other: LineRange): Boolean = {
    (from < other.from && to > other.from) ||
      (from < other.to && to > other.from)
  }

  def intersect(other: LineRange): Option[LineRange] = {
    if(overlap(other)) Some(LineRange(from.max(other.from), to.min(other.to)))
    else None
  }

  def toLines: Seq[LineIdx] = {
    (from.i until to.i).map(LineIdx.apply)
  }

  def middleLine: Option[LineIdx] = {
    if (from ==== to) None
    else Some(LineIdx((from.i + to.i) / 2))
  }

  def size: Int = to.i - from.i
}
object LineChangePoint {
  def from(left: (Int, Int), right: (Int, Int)) =
    LineChangePoint(LineRange(LineIdx(left._1), LineIdx(left._2)), LineRange(LineIdx(right._1), LineIdx(right._2)))
}

/**
  * If there is a zero length range that means that the insertion is just before the position
  *
  *
  */
final case class LineChangePoint(left: LineRange, right: LineRange) {
  def positive: Boolean = left.positive || right.positive
  def withoutRight(line: LineIdx): Seq[LineChangePoint] = {
    right.without(line).map(LineChangePoint(left, _))
  }
  def withoutLeft(line: LineIdx): Seq[LineChangePoint] = {
    left.without(line).map(LineChangePoint(_, right))
  }
  def intersect(oLeft: LineRange, oRight: LineRange): Option[LineChangePoint] = {
    (left.intersect(oLeft), right.intersect(oRight)) match {
      case (Some(l), Some(r)) => Some(LineChangePoint(l, r))
      case _ => None
    }
  }
  def overlap(oLeft: LineRange, oRight: LineRange): Boolean = {
    oLeft.overlap(left) || oRight.overlap(right)
  }

}


object InsertionPointCalculator {

  private def handleRemains(leftUnmatched: Traversable[LineRange], rightUnmatched: Traversable[LineRange],
                            allMatches: Traversable[LineMatch]): Traversable[LineChangePoint] = {

    val allSortedLeft = allMatches.toSeq.sortBy(_.leftLineIdx)
    val allSortedRight = allMatches.toSeq.sortBy(_.rightLineIdx)
    val leftBeforeMatches = leftUnmatched.map { lu =>
      allSortedLeft.filter(_.leftLineIdx < lu.from).lastOption.getOrElse(LineMatch(LineIdx(-1), LineIdx(-1))) -> lu
    }

    val rightBeforeMatches = rightUnmatched.map { ru =>
      allSortedRight.filter(_.rightLineIdx < ru.from).lastOption.getOrElse(LineMatch(LineIdx(-1), LineIdx(-1))) -> ru
    }

    val leftEqs = leftBeforeMatches.map { case (lineMatch, leftRange) =>
      val nextRight = lineMatch.rightLineIdx + 1
      LineChangePoint(
        leftRange,
        LineRange(nextRight, rightUnmatched.find(_.from ==== nextRight).map(_.to).getOrElse(nextRight))
      )
    }

    val rightEqs = rightBeforeMatches.map { case (lineMatch, rightRange) =>
      val nextLeft = lineMatch.leftLineIdx + 1
      LineChangePoint(
        LineRange(nextLeft, leftUnmatched.find(_.from ==== nextLeft).map(_.to).getOrElse(nextLeft)),
        rightRange
      )
    }
    leftEqs ++ rightEqs
  }

  def calc(notMoved: Traversable[LineMatch], moved: Traversable[LineMatch], leftLineCount: Int, rightLineCount: Int): Traversable[LineChangePoint] = {
    // sorting by left imply sorting by right
    val notMovedSorted = notMoved.toList.sortBy(_.leftLineIdx) :+ LineMatch.create(leftLineCount, rightLineCount)
    @SuppressWarnings(Array(Var))
    var last = LineMatch.create(-1, -1)
    val builder = Seq.newBuilder[LineChangePoint]
    // this could be done with sliding though I am not convinced it would be any better
    notMovedSorted.foreach { current =>
      val eq = LineChangePoint(LineRange(last.leftLineIdx + 1, current.leftLineIdx), LineRange(last.rightLineIdx + 1, current.rightLineIdx))
      if (eq.positive)
        discard(builder += eq)
      last = current
    }

    val resultWithMovedFilteredOut = {
      val resultMovedNotFilteredOut = builder.result()

      moved.foldLeft(resultMovedNotFilteredOut) { case (eqs, movedLine) =>
        eqs.flatMap(_.withoutRight(movedLine.rightLineIdx).flatMap(_.withoutLeft(movedLine.leftLineIdx)))
      }
    }

    val leftAll = LineRange(LineIdx(0), LineIdx(leftLineCount))
    val rightAll = LineRange(LineIdx(0), LineIdx(rightLineCount))
    val matched = notMoved ++ moved
    val leftUnmatched = {
      val leftProcessed = matched.map(_.leftLineIdx) ++ resultWithMovedFilteredOut.flatMap(_.left.toLines)

      leftProcessed.foldLeft(Traversable(leftAll)) { (acc, leftLine) =>
        acc.flatMap(_.without(leftLine))
      }.filter(_.positive)
    }

    val rightUnmatched = {
      val rightProcessed = matched.map(_.rightLineIdx) ++ resultWithMovedFilteredOut.flatMap(_.right.toLines)
      rightProcessed.foldLeft(Traversable(rightAll)) { (acc, rightLine) =>
        acc.flatMap(_.without(rightLine))
      }.filter(_.positive)
    }
    (handleRemains(leftUnmatched, rightUnmatched, notMoved ++ moved) ++ resultWithMovedFilteredOut).toSeq.distinct
  }
}