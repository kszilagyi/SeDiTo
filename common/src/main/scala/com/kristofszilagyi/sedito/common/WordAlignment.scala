package com.kristofszilagyi.sedito.common

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.kristofszilagyi.sedito.common.AmbiguousWordAlignment.resolveConflicts
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import info.debatty.java.stringsimilarity.Levenshtein


object Selection {
  def create(line: String, lineIdx: LineIdx, from: CharIdxInLine, toExcl: CharIdxInLine): Validated[WordIndexRangeError, Selection] = {
    if (from.i < 0 || from.i >= line.length) Invalid(IndexIsOutOfRange(from.i, line))
    else if (toExcl.i < 0 || toExcl.i > line.length) Invalid(IndexIsOutOfRange(toExcl.i, line))
    else if (from.i >= toExcl.i) Invalid(RangeIsNotPositive(from.i, toExcl.i, line))
    else Valid(new Selection(line, lineIdx, from, toExcl) {})
  }
}
//I am using this instead of indexing into the whole string so that line ending types do not make a difference
sealed abstract case class Selection private(line: String, lineIdx: LineIdx, from: CharIdxInLine, toExcl: CharIdxInLine) {
  def toText: String = s"${line.substring(from.i, toExcl.i)}"

  override def toString: String = {
    s"${lineIdx.i}: ${from.i} - ${toExcl.i} [$toText]"
  }

  def toIndexRangeWithinLine: Validated[WordIndexRangeError, WordIndexRange] = {
    WordIndexRange.create(startIncl = from.i, endExcl = toExcl.i, fullText = FullText(line))
  }
}
final case class WordMatch(left: Selection, right: Selection) {
  def readable: String = {
    s"${left.toText} - ${right.toText}"
  }
}

object AmbiguousWordAlignment {

  private final case class Ld(left: WordIndexRange, right: WordIndexRange, dist: Double)
  private final case class PossibleResult(result: Set[Ld])

  //TODO this now can fail on very long lines (though I think these are only called when reading test case?)
  @SuppressWarnings(Array(Warts.Recursion))
  private def approximatePossibleBestMatches(orderedLds: List[Ld], result: Set[Ld]): Set[PossibleResult] = {
    orderedLds match {
      case first :: rest =>
        val conflicts = rest.filter { r =>
          r.left ==== first.left || r.right ==== first.right
        }
        val conflictWithSameLd = conflicts.filter(_.dist ==== first.dist)
        val potentials = conflictWithSameLd :+ first
        val possibleResults = potentials.map { pot =>
          val withoutConflict = orderedLds.filterNot { r =>
            r.left ==== pot.left || r.right ==== pot.right
          }
          approximatePossibleBestMatches(withoutConflict, result + pot)
        }.flatten
        possibleResults.toSet
      case Nil => Set(PossibleResult(result))
    }
  }

  private def findResultWithLeastMoves(results: Set[PossibleResult]) = {
    results.map { r =>
      val leftSorted = r.result.toSeq.sortBy(_.left.startIncl)
      val rightOrder = leftSorted.map(_.right.startIncl)
      r -> LongestIncreasingSubsequence.apply(rightOrder.toArray).size
    }.toSeq.sortBy(_._2).map(_._1).lastOption
  }

  private def wordMatches(left: Lines, right: Lines, matches: Set[LineMatch]) = {
    val allMatches = matches.flatMap { m =>
      val leftLine = left.l(m.leftLineIdx.i) //replace with .get?
      val rightLine = right.l(m.rightLineIdx.i)
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

      val possibleResults = approximatePossibleBestMatches(sortedLds.toList, Set.empty)
      val bestLdSet = findResultWithLeastMoves(possibleResults).map(_.result).getOrElse(Set.empty)

      val newMatchesForLine = bestLdSet.map { ld =>
        WordMatch(
          Selection.create(leftLine, m.leftLineIdx, CharIdxInLine(ld.left.startIncl), CharIdxInLine(ld.left.endExcl)).getAssert("invalid range"),
          Selection.create(rightLine, m.rightLineIdx, CharIdxInLine(ld.right.startIncl), CharIdxInLine(ld.right.endExcl)).getAssert("invalid range")
        )
      }
      newMatchesForLine
    }
    allMatches
  }

  def fromOld(left: Lines, right: Lines, alignment: AmbiguousLineAlignment): AmbiguousWordAlignment = {
    AmbiguousWordAlignment(wordMatches(left, right, alignment.matches))
  }

  private def sortMatches(matches: Traversable[WordMatch]) = {
    matches.toSeq.sortBy(m => (m.left.lineIdx, m.right.lineIdx, m.left.from, m.right.from)) //arbitrary but deterministic ordering
  }

  @SuppressWarnings(Array(Warts.TraversableOps))
  private def resolveConflicts(conflictMap: Map[(LineIdx, CharIdxInLine, CharIdxInLine), Traversable[WordMatch]]) = {
    conflictMap.map { case (_, conflictingMatches) =>
      sortMatches(conflictingMatches).head //this is safe because groupBy will never result in an empty list
    }
  }
}
/**
  * For explanation see the comment on AmbiguousLineAlignment
  */
final case class AmbiguousWordAlignment(matches: Set[WordMatch]) {
  def readable: String = matches.map(_.readable).mkString(", ")

  def toUnambigous: UnambiguousWordAlignment = {
    //we assume no overlap. So every conflict is caused by a word being matched with multiple other words
    val leftMap = matches.groupBy(m => (m.left.lineIdx, m.left.from, m.left.toExcl))
    val leftResolved = resolveConflicts(leftMap)
    val rightMap = leftResolved.groupBy(m => (m.right.lineIdx, m.right.from, m.right.toExcl))
    val botResolved = resolveConflicts(rightMap)
    UnambiguousWordAlignment(botResolved.toSet)
  }
}

//I am undecided if this should check for conflicts or not. Same for UnambiguousLineAlignment
final case class UnambiguousWordAlignment(matches: Set[WordMatch]) {
  def readable: String = matches.map(_.readable).mkString(", ")
}