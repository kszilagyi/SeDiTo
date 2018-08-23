package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.LineMatch

object Scroller {
  sealed trait ScrollAlignment
  case object Aligned extends ScrollAlignment
  case object LeftIsLower extends ScrollAlignment
  case object RightIsLower extends ScrollAlignment
  case object NothingOnScreen extends ScrollAlignment

  def calc(leftVisible: LineRange, rightVisible: LineRange, notMoved: Set[LineMatch]): ScrollAlignment = {
    (leftVisible.middleLine, rightVisible.middleLine) match {
      case (Some(middleLeft), Some(middleRight)) =>
        val maybeMiddleLeftPair = notMoved.flatMap(LineMatch.unapply).toMap.get(middleLeft)
        maybeMiddleLeftPair match {
          case Some(middleLeftPair) =>
            if (middleLeftPair < middleRight) LeftIsLower
            else if(middleLeftPair > middleRight) RightIsLower
            else Aligned
          case None => Aligned //todo implement
        }
      case _ => NothingOnScreen
    }
  }

}
