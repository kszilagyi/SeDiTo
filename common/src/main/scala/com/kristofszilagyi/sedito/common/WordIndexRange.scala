package com.kristofszilagyi.sedito.common

sealed trait WordIndexRangeError
final case class IndexIsOutOfRange(idx: Int, s: String) extends WordIndexRangeError
final case class RangeIsNotPositive(from: Int, to: Int, s: String) extends WordIndexRangeError
