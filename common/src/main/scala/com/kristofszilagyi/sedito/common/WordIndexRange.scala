package com.kristofszilagyi.sedito.common

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import scala.collection.Searching._

/**
  * the only point of this class is optimisation
  */
final case class FullText(s: String) {
  lazy val lineBreakIdxes: Array[Int] = s.zipWithIndex.filter(_._1 ==== '\n').map(_._2).toArray.sorted
}
sealed abstract case class WordIndexRange private(startIncl: Int, endExcl: Int, fullText: FullText) {
  def toWord: String = fullText.s.substring(startIncl, endExcl)

  override def toString: String = s"($startIncl, $endExcl) [$toWord]"

  //note we are not worrying about the case when there \n within the range
  //as you can't create a range like that
  def toSelection: Selection = {
    //todo line ending types

    val lineBreakBeforeInArray = math.abs(fullText.lineBreakIdxes.search(startIncl).insertionPoint) - 1
    val lineBreakAfterInArray = lineBreakBeforeInArray + 1
    val lineBreakAfterInText =
      if (lineBreakAfterInArray < fullText.lineBreakIdxes.length) fullText.lineBreakIdxes(lineBreakAfterInArray)
      else fullText.s.length
    val lineIdx = LineIdx(lineBreakBeforeInArray + 1)
    val lineBreakBeforeInText =
      if (lineBreakBeforeInArray >= 0) fullText.lineBreakIdxes(lineBreakBeforeInArray)
      else -1
    val line = fullText.s.substring(lineBreakBeforeInText + 1, lineBreakAfterInText)
    val from = CharIdxInLine(startIncl - (lineBreakBeforeInText + 1))
    val to = CharIdxInLine(endExcl - (lineBreakBeforeInText + 1))
    Selection.create(line, lineIdx, from, to).getAssert
  }
}

sealed trait WordIndexRangeError
final case class IndexIsOutOfRange(idx: Int, s: String) extends WordIndexRangeError
final case class RangeIsNotPositive(from: Int, to: Int, s: String) extends WordIndexRangeError
final case class NotAWord(from: Int, to: Int, s: String) extends WordIndexRangeError

object WordIndexRange {
  def create(startIncl: Int, endExcl: Int, fullText: FullText): Validated[WordIndexRangeError, WordIndexRange]= {
    if (startIncl < 0 || startIncl >= fullText.s.length) Invalid(IndexIsOutOfRange(startIncl, fullText.s))
    else if (endExcl < 0 || endExcl > fullText.s.length) Invalid(IndexIsOutOfRange(endExcl, fullText.s))
    else if(startIncl >= endExcl) Invalid(RangeIsNotPositive(startIncl, endExcl, fullText.s))
    else {
      val range = new WordIndexRange(startIncl, endExcl, fullText) {}
      if (range.toWord.matches("\\s")) {
        Invalid(NotAWord(startIncl, endExcl, fullText.s))
      } else {
        Valid(range)
      }
    }
  }
}
