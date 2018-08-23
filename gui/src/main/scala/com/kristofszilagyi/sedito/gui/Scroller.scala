package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.LineIdx

import scala.collection.immutable.TreeMap

object Scroller {
  sealed trait ScrollAlignment
  case object Aligned extends ScrollAlignment
  case object LeftIsLower extends ScrollAlignment
  case object RightIsLower extends ScrollAlignment
  case object NothingOnScreen extends ScrollAlignment

  private def closest(tree: TreeMap[LineIdx, LineIdx], l: LineIdx) = {
    val maybeLower = tree.to(l).lastOption
    val maybeUpper = tree.from(l).headOption
    val closestPair = (maybeLower, maybeUpper) match {
      case (Some(lower), Some(upper)) =>
        if (math.abs(lower._1.i - l.i) < math.abs(upper._1.i - l.i)) Some(lower)
        else Some(upper)
      case (Some(lower), None) => Some(lower)
      case (None, Some(upper)) => Some(upper)
      case (None, None) => None
    }
    closestPair
  }
  def calc(leftVisible: LineRange, rightVisible: LineRange, notMoved: TreeMap[LineIdx, LineIdx]): ScrollAlignment = {
    (leftVisible.middleLine, rightVisible.middleLine) match {
      case (Some(middleLeft), Some(middleRight)) =>
        val maybeClosestPair = closest(notMoved, middleLeft)
        maybeClosestPair match {
          case Some(closestPair) =>
            val leftDistanceFromMiddle = closestPair._1.i - middleLeft.i
            val rightDistanceFromMiddle = closestPair._2.i - middleRight.i
            if (leftDistanceFromMiddle > rightDistanceFromMiddle) LeftIsLower
            else if (leftDistanceFromMiddle < rightDistanceFromMiddle) RightIsLower
            else Aligned
          case None => Aligned //the best we can do
        }
      case _ => NothingOnScreen
    }
  }

}
