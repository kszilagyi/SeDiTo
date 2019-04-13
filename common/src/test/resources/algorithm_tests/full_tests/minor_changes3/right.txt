package com.kristofszilagyi.sedito.common

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import info.debatty.java.stringsimilarity.Levenshtein

import scala.annotation.tailrec


object Selection {
  def create(line: String, lineIdx: LineIdx, from: CharIdxInLine, toExcl: CharIdxInLine): Validated[RangeError, Selection] = {
    if (from.i < 0 || from.i >= line.length) Invalid(IndexIsOutOfRange(from.i, line))
    else if (toExcl.i < 0 || toExcl.i > line.length) Invalid(IndexIsOutOfRange(from.i, line))
    else if (from.i >= toExcl.i) Invalid(RangeIsNotPositive(from.i, toExcl.i, line))
    else Valid(new Selection(line, lineIdx, from, toExcl) {})
  }
}
//I am using this instead of indexing into the whole string so that line ending types do not make a difference
sealed abstract case class Selection private(line: String, lineIdx: LineIdx, from: CharIdxInLine, toExcl: CharIdxInLine) {
  def readable: String = s"${line.substring(from.i, toExcl.i)}"

  override def toString: String = {
    s"${lineIdx.i}: ${from.i} - ${toExcl.i} [$readable]"
  }
}
final case class WordMatch(left: Selection, right: Selection) {
  def readble: String = {
    s"${left.readable} - ${right.readable}"
  }
}

object WordAlignment {

  private final case class Ld(left: WordIndexRange, right: WordIndexRange, dist: Double)

  //done properly this would use the Hungarian algo. But that's too hard
  @tailrec
  private def approximateBestMatches(orderedLds: List[Ld], result: Set[Ld]): Set[Ld] = {
    orderedLds match {
      case first :: rest =>
        val notConflictingRest = rest.filterNot { r =>
          r.left ==== first.left || r.right ==== first.right
        }
        approximateBestMatches(notConflictingRest ,result + first)
      case Nil => result
    }
  }

  def fromOld(left: IndexedSeq[String], right: IndexedSeq[String], alignment: LineAlignment): WordAlignment = {
    val allMatches = alignment.matches.flatMap { m =>
      val leftLine = left(m.leftLineIdx.i)
      val rightLine = right(m.rightLineIdx.i)
      val leftWordRanges = Wordizer.toWordIndices(leftLine)
      val rightWordRanges = Wordizer.toWordIndices(rightLine)
      val ldCalculator = new Levenshtein()
      val lds = leftWordRanges.flatMap { leftRange =>
        rightWordRanges.map { rightRange =>
          val leftWord = leftRange.toWord
          val rightWord = rightRange.toWord
          Ld(leftRange, rightRange, ldCalculator.distance(leftWord, rightWord))
        }
      }

      val sortedLds = lds.filter{ ld =>
        ld.dist <= (ld.left.toWord.length + ld.right.toWord.length) / 2 / 3
      }.sortBy(_.dist)

      val matches = approximateBestMatches(sortedLds.toList, Set.empty)

      val newMatchesForLine = matches.map { ld =>
        WordMatch(
          Selection.create(leftLine, m.leftLineIdx, CharIdxInLine(ld.left.startIncl), CharIdxInLine(ld.left.endExcl)).getAssert("invalid range"),
          Selection.create(rightLine, m.rightLineIdx, CharIdxInLine(ld.right.startIncl), CharIdxInLine(ld.right.endExcl)).getAssert("invalid range")
        )
      }
      newMatchesForLine
    }
    WordAlignment(allMatches)
  }
}
final case class WordAlignment(matches: Set[WordMatch]) {
  def readble: String = matches.map { m =>
    m.readble
  }.mkString(", ")
}