package com.kristofszilagyi.sedito.common

import org.bitbucket.cowwoc.diffmatchpatch.DiffMatchPatch.Operation

sealed trait LineEditType


final case class LineMoved(fromTo: LineIdx) extends LineEditType
case object LineInserted extends LineEditType
case object LineDeleted extends LineEditType
case object LineSame extends LineEditType


object CharEditType {
  def from(op: Operation): CharEditType = {
    op match {
      case Operation.DELETE => CharsDeleted
      case Operation.INSERT => CharsInserted
      case Operation.EQUAL => CharsSame
    }
  }
}

sealed trait CharEditType

/**
  * @param edits extra edits above the move
  */
final case class CharsMoved(fromTo: Selection, edits: Traversable[CharEdit]) extends CharEditType
case object CharsInserted extends CharEditType
case object CharsDeleted extends CharEditType
case object CharsSame extends CharEditType
