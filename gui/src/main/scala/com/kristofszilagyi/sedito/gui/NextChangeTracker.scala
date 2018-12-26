package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.{LineIdx, Warts}

sealed trait HaveMore
case object StillHaveMore extends HaveMore
case object ThereIsNoMore extends HaveMore

final class NextChangeTracker(eqPoints: Traversable[EquivalencePoint]) {
  private val orderedByLeft: Seq[EquivalencePoint] = eqPoints.toSeq.sortBy(_.left.from)
  assert(eqPoints.forall(_.positive))

  @SuppressWarnings(Array(Warts.Var))
  private var position = 0

  private def current() = {
    orderedByLeft.lift(position)
  }
  def left(): LineIdx = {
    current().map(_.left.from).getOrElse(LineIdx(0))
  }

  def right(): LineIdx = {
    current().map(_.right.from).getOrElse(LineIdx(0))
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