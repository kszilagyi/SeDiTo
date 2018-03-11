package com.kristofszilagyi.sedito.common

import org.bitbucket.cowwoc.diffmatchpatch.DiffMatchPatch.Operation


object EditType {
  def from(op: Operation): EditType = {
    op match {
      case Operation.DELETE => Deleted
      case Operation.INSERT => Inserted
      case Operation.EQUAL => Same
    }
  }
}

sealed trait EditType

final case class Moved(from: LineIdx) extends EditType
case object Inserted extends EditType
case object Deleted extends EditType
case object Same extends EditType


