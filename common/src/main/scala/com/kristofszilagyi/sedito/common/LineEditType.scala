package com.kristofszilagyi.sedito.common

import org.bitbucket.cowwoc.diffmatchpatch.DiffMatchPatch.Operation

sealed trait LineEditType


final case class LineMoved(fromTo: LineIdx) extends LineEditType
case object LineInserted extends LineEditType
case object LineDeleted extends LineEditType
case object LineSame extends LineEditType


object CharEditType {
  def from(op: Operation): ApplicableCharEditType = {
    op match {
      case Operation.DELETE => CharsDeleted
      case Operation.INSERT => CharsInserted
      case Operation.EQUAL => CharsSame
    }
  }
}

sealed trait CharEditType

sealed trait ApplicableCharEditType extends CharEditType

/**
  * @param edits extra edits above the move
  */
final case class CharsMoved(fromTo: Selection, edits: Traversable[DirectCharEdit]) extends CharEditType
case object CharsInserted extends ApplicableCharEditType
case object CharsDeleted extends ApplicableCharEditType
case object CharsSame extends ApplicableCharEditType
