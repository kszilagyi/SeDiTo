package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.{LineIdx, Warts}

sealed trait HaveMore
case object StillHaveMore extends HaveMore
case object ThereIsNoMore extends HaveMore

/**
  * A change point contains both line changes (line add/remove/move) + char changes
  */
final case class ChangePointStart(left: LineIdx, right: LineIdx)

final class NextChangeTracker(changePointStarts: Traversable[ChangePointStart]) {
  private val orderedByLeft: Seq[ChangePointStart] = changePointStarts.toSeq.sortBy(_.left)

  @SuppressWarnings(Array(Warts.Var))
  private var position = 0

  private def current() = {
    orderedByLeft.lift(position)
  }
  def left(): LineIdx = {
    current().map(_.left).getOrElse(LineIdx(0))
  }

  def right(): LineIdx = {
    current().map(_.right).getOrElse(LineIdx(0))
  }

  def next(): Unit = {
    if (hasNext()) {
      position += 1
    }
  }

  def prev(): Unit = {
    if (hasPrev()) {
      position -= 1
    }
  }

  def hasNext(): Boolean = {
    position + 1 < orderedByLeft.size
  }

  def hasPrev(): Boolean = {
    position - 1 >= 0
  }

}