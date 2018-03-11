package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.{LineIdx, Match}
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
final case class NumberOfLinesPadding(i: Int)

sealed trait Side
case object Left extends Side
case object Right extends Side

/**
  * The padding is above the line
  */
final case class PaddingResult(side: Side, line: LineIdx, amount: NumberOfLinesPadding)

object PaddingCalculator {
  private final case class LastIndex(left: LineIdx, right: LineIdx)
  /**
    *
    * @param matches should be the maximum non-crossing matches. So if sorted on one side the other side will be sorted too
    */
  def calc(matches: Set[Match], maxLeft: LineIdx, maxRight: LineIdx): List[PaddingResult] = {
    val sortedMatches = matches.toSeq.sortBy(_.leftLineIdx.i)

    val foldRes = sortedMatches.foldLeft((List.empty[PaddingResult], LastIndex(LineIdx(-1), LineIdx(-1)))) { case ((result, LastIndex(lastLeft, lastRight)), m) =>
      val leftDiff = m.leftLineIdx.i - lastLeft.i - 1
      val leftRes = if (leftDiff !=== 0) {
        Some(PaddingResult(Right, m.rightLineIdx, NumberOfLinesPadding(leftDiff)))
      } else None
      val rightDiff = m.rightLineIdx.i - lastRight.i - 1
      val rightRes = if (rightDiff !=== 0) {
        Some(PaddingResult(Left, m.leftLineIdx, NumberOfLinesPadding(rightDiff)))
      } else None

      (result ++ leftRes.toList ++ rightRes.toList, LastIndex(m.leftLineIdx, m.rightLineIdx))
    }

    val lastIndex = foldRes._2
    val finalPadding = if (lastIndex.left < maxLeft && lastIndex.right < maxRight) {
      val leftDiff = maxLeft.i - lastIndex.left.i
      Some(PaddingResult(Right, lastIndex.right + 1, NumberOfLinesPadding(leftDiff)))
    } else None
    foldRes._1 ++ finalPadding.toList
  }
}
