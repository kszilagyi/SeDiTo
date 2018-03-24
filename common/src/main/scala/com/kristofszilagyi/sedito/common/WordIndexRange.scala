package com.kristofszilagyi.sedito.common

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}


sealed abstract case class WordIndexRange private(startIncl: Int, endExcl: Int, s: String) {
  def toWord: String = s.substring(startIncl, endExcl)

  override def toString: String = s"($startIncl, $endExcl) [$toWord]"
}

sealed trait RangeError
final case class IndexIsOutOfRange(idx: Int, s: String) extends RangeError
final case class RangeIsNotPositive(from: Int, to: Int, s: String) extends RangeError

object WordIndexRange {
  def create(startIncl: Int, endExcl: Int, s: String): Validated[RangeError, WordIndexRange]= {
    if (startIncl < 0 || startIncl >= s.length) Invalid(IndexIsOutOfRange(startIncl, s))
    else if (endExcl < 0 || endExcl > s.length) Invalid(IndexIsOutOfRange(endExcl, s))
    else if(startIncl >= endExcl) Invalid(RangeIsNotPositive(startIncl, endExcl, s))
    else Valid(new WordIndexRange(startIncl, endExcl, s) {})
  }
}
