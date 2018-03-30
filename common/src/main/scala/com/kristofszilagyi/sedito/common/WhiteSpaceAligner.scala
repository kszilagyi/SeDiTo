package com.kristofszilagyi.sedito.common

import scala.annotation.tailrec

object Lines {
  def empty: Lines = Lines(IndexedSeq.empty)
}

final case class Lines(l: IndexedSeq[String]) {
  def inBounds(lineIdx: LineIdx): Boolean = {
    lineIdx.i < l.size && lineIdx.i >= 0
  }

  def get(lineIdx: LineIdx): Option[String] = {
    if (inBounds(lineIdx)) Some(l(lineIdx.i))
    else None
  }
}
/**
  * White spaces are not taking part of the ordinary matching process
  * Therefore we need to infer their alignment based on the normal matches
  */
object WhiteSpaceAligner {


  /**
    * warning: alignment is both the accumulator and the thing which we can't have conflict with
    */
  @tailrec
  private def extendMatch(up: Boolean, m: LineMatch, left: Lines, right: Lines, alignment: LineAlignment): LineAlignment = {
    val dirInt = if (up) -1 else 1
    val next = LineMatch(m.leftLineIdx + dirInt, m.rightLineIdx + dirInt)
    (left.get(next.leftLineIdx), right.get(next.rightLineIdx)) match {
      case (Some(nextLeft), Some(nextRight)) =>
        if (nextLeft.trim.isEmpty && nextRight.trim.isEmpty && !alignment.conflict(next)) {
          extendMatch(up, next, left, right, alignment.withMatch(next))
        } else {
          alignment
        }
      case _ => alignment
    }


  }
  def align(left: Lines, right: Lines, alignment: LineAlignment): LineAlignment = {
    alignment.matches.foldLeft(alignment) { case (currentAlignment, m) =>
      val upExtra = extendMatch(up = true, m, left, right, currentAlignment)
      val bothExtra = extendMatch(up = false, m, left, right, upExtra)
      bothExtra
    }
  }

}
