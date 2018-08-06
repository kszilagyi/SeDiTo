package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common.{LineIdx, LineMatch}
final case class LineRange(from: LineIdx, to: LineIdx) {
  def positive: Boolean = to.i - from.i > 0
  def without(line: LineIdx): Seq[LineRange] = {
    if (line < from || line >= to) Seq(this)
    else {
      val left = LineRange(from, line)
      val right = LineRange(line + 1, to)
      if (!left.positive && !right.positive) Seq(left)
      else Seq(left, right).filter(_.positive)
    }
  }

  def overlap(other: LineRange): Boolean = {
    (from < other.from && to > other.from) ||
      (from < other.to && to > other.from)
  }

  def toLines: Seq[LineIdx] = {
    (from.i until to.i).map(LineIdx.apply)
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

  private def handleRemains(leftUnmatched: Traversable[LineRange], rightUnmatched: Traversable[LineRange],
                            allMatches: Traversable[LineMatch]): Traversable[EquivalencePoint] = {
//    val leftAll = LineRange(LineIdx(0), LineIdx(leftLineCount))
//    val rightAll = LineRange(LineIdx(0), LineIdx(rightLineCount))
//    val allMatches = notMoved ++ moved
//    val leftMatched = allMatches.map(_.leftLineIdx)
//    val rightMatched = allMatches.map(_.rightLineIdx)
//
//    val leftUnmatched = leftMatched.foldLeft(Traversable(leftAll)) { (acc, leftLine) =>
//      acc.flatMap(_.without(leftLine))
//    }.filter(_.positive)
//    val rightUnmatched = rightMatched.foldLeft(Traversable(rightAll)) { (acc, rightLine) =>
//      acc.flatMap(_.without(rightLine))
//    }.filter(_.positive)

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
      EquivalencePoint(
        leftRange,
        LineRange(nextRight, rightUnmatched.find(_.from ==== nextRight).map(_.to).getOrElse(nextRight))
      )
    }

    val rightEqs = rightBeforeMatches.map { case (lineMatch, rightRange) =>
      val nextLeft = lineMatch.leftLineIdx + 1
      EquivalencePoint(
        LineRange(nextLeft, leftUnmatched.find(_.from ==== nextLeft).map(_.to).getOrElse(nextLeft)),
        rightRange
      )
    }
    leftEqs ++ rightEqs
  }

  def calc(notMoved: Traversable[LineMatch], moved: Traversable[LineMatch], leftLineCount: Int, rightLineCount: Int): Traversable[EquivalencePoint] = {
    // sorting by left imply sorting by right
    val notMovedSorted = notMoved.toList.sortBy(_.leftLineIdx) :+ LineMatch.create(leftLineCount, rightLineCount)
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
    val leftAll = LineRange(LineIdx(0), LineIdx(leftLineCount))
    val rightAll = LineRange(LineIdx(0), LineIdx(rightLineCount))
    val matched = notMoved ++ moved
    val leftProcessed = matched.map(_.leftLineIdx) ++ eqWoMoves.flatMap(_.left.toLines)
    val leftUnmatched = leftProcessed.foldLeft(Traversable(leftAll)) { (acc, leftLine) =>
      acc.flatMap(_.without(leftLine))
    }.filter(_.positive)

    val rightProcessed = matched.map(_.rightLineIdx) ++ eqWoMoves.flatMap(_.right.toLines)
    val rightUnmatched = rightProcessed.foldLeft(Traversable(rightAll)) { (acc, rightLine) =>
      acc.flatMap(_.without(rightLine))
    }.filter(_.positive)
    (handleRemains(leftUnmatched, rightUnmatched, notMoved ++ moved) ++ eqWoMoves).toSeq.distinct
//    val selfMap = eqWoMoves.zip(Seq(eqWoMoves)).toMap
//    val originalWithReduced = moved.foldLeft(selfMap) { case (eqs, movedLine) =>
//      eqs.mapValuesNow(_.flatMap(_.withoutRight(movedLine.rightLineIdx).flatMap(_.withoutLeft(movedLine.leftLineIdx))))
//    }
  }
}
