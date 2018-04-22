package com.kristofszilagyi.sedito.common

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated

sealed abstract case class WordIndexRange private(startIncl: Int, endExcl: Int, s: String) {
  def toWord: String = s.substring(startIncl, endExcl)

  override def toString: String = s"($startIncl, $endExcl) [$toWord]"

  //note we are not worrying about the case when there \n within the range
  //as you can't create a range like that
  def toSelection: Selection = {
    //todo line ending types
    val lineBreakIdxes = s.zipWithIndex.filter(_._1 ==== '\n').map(_._2)
    val lineBreaksBefore = lineBreakIdxes.filter(_ < startIncl)
    val lineBreakAfter = lineBreakIdxes.find(_ >= endExcl).getOrElse(s.length)
    val lineIdx = LineIdx(lineBreaksBefore.size)
    val lineBreakBefore = lineBreaksBefore.lastOption.getOrElse(-1)
    val line = s.substring(lineBreakBefore + 1, lineBreakAfter)
    val from = CharIdxInLine(startIncl - (lineBreakBefore + 1))
    val to = CharIdxInLine(endExcl - (lineBreakBefore + 1))
    Selection.create(line, lineIdx, from, to).getAssert
  }
}

sealed trait WordIndexRangeError
final case class IndexIsOutOfRange(idx: Int, s: String) extends WordIndexRangeError
final case class RangeIsNotPositive(from: Int, to: Int, s: String) extends WordIndexRangeError
final case class NotAWord(from: Int, to: Int, s: String) extends WordIndexRangeError

object WordIndexRange {
  def create(startIncl: Int, endExcl: Int, s: String): Validated[WordIndexRangeError, WordIndexRange]= {
    if (startIncl < 0 || startIncl >= s.length) Invalid(IndexIsOutOfRange(startIncl, s))
    else if (endExcl < 0 || endExcl > s.length) Invalid(IndexIsOutOfRange(endExcl, s))
    else if(startIncl >= endExcl) Invalid(RangeIsNotPositive(startIncl, endExcl, s))
    else {
      val range = new WordIndexRange(startIncl, endExcl, s) {}
      if (range.toWord.matches("\\s")) {
        Invalid(NotAWord(startIncl, endExcl, s))
      } else {
        Valid(range)
      }
    }
  }
}
