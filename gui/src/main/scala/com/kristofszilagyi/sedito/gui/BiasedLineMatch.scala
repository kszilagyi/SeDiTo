package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.{LineIdx, LineMatch}

object BiasedLineMatch {
  def left(lineMatch: LineMatch): BiasedLineMatch = BiasedLineMatch(lineMatch.leftLineIdx, lineMatch.rightLineIdx)
  def right(lineMatch: LineMatch): BiasedLineMatch = BiasedLineMatch(lineMatch.rightLineIdx, lineMatch.leftLineIdx)
}
final case class BiasedLineMatch(thisSide: LineIdx, otherSide: LineIdx)