package com.kristofszilagyi.sedito.common

import info.debatty.java.stringsimilarity.Levenshtein

import scala.annotation.tailrec
import TypeSafeEqualsOps._

//I am using this instead of indexing into the whole string so that line ending types do not make a difference
final case class Selection(line: String, lineIdx: LineIdx, from: CharIdxInLine, toExcl: CharIdxInLine) {
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
          Selection(leftLine, m.leftLineIdx, CharIdxInLine(ld.left.startIncl), CharIdxInLine(ld.left.endExcl)),
          Selection(rightLine, m.rightLineIdx, CharIdxInLine(ld.right.startIncl), CharIdxInLine(ld.right.endExcl))
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