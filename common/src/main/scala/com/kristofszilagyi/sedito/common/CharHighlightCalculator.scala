package com.kristofszilagyi.sedito.common

import org.bitbucket.cowwoc.diffmatchpatch.DiffMatchPatch
import org.bitbucket.cowwoc.diffmatchpatch.DiffMatchPatch.Operation
import TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import AssertionEx.fail
import scala.collection.JavaConverters._

/**
  * This one can contain moves too but DirectCharEdit can't
  */
final case class CharEdit(from: CharIdxInLine, to: CharIdxInLine, editType: CharEditType) {
  def text(line: String): String = {
    line.substring(from.i, to.i)
  }
}


final case class DirectCharEdit(from: CharIdxInLine, to: CharIdxInLine, editType: ApplicableCharEditType) {
  def text(line: String): String = {
    line.substring(from.i, to.i)
  }
  def toCharEdit: CharEdit = CharEdit(from, to, editType)
}

final case class CharHighlight(left: Map[LineIdx, Traversable[CharEdit]], right: Map[LineIdx, Traversable[CharEdit]])

object CharHighlightCalculator {
  private trait MatchSide
  private case object LeftMatch extends MatchSide
  private case object RightMatch extends MatchSide
  private case object BothMatch extends MatchSide

  private def toPositions(baseline: Selection, diffs: Seq[DiffMatchPatch.Diff]) = {
    diffs.foldLeft(Seq.empty[DirectCharEdit]) { case (result, diff) =>
      val op = diff.operation
      val len = diff.text.length
      val lastPos = result.lastOption.map(_.to).getOrElse(CharIdxInLine(0))
      val to = lastPos + len
      result :+ DirectCharEdit(from = baseline.from + lastPos, to = baseline.from + to, editType = CharEditType.from(op))
    }
  }

  private def wordsWithoutMatch(line: String, wordMatchesInEitherLine: Set[Selection]) = {
    val words = Wordizer.toWordIndices(line).toSet
    val wordsWithMatch = wordMatchesInEitherLine.map(_.toIndexRange.getAssert(""))
    words -- wordsWithMatch
  }

  private def merge(edits1: Set[(LineIdx, Set[CharEdit])], edits2: Set[(LineIdx, Set[CharEdit])]) = {
    (edits1.toSeq ++ edits2.toSeq).groupBy(_._1).map{ case (idx, idxesAndEdits) =>
      idx -> idxesAndEdits.flatMap { case (_, editsOnly) => editsOnly }.toSet
    }
  }

  private sealed class MoveOrSame
  private case object M extends MoveOrSame
  private case object S extends MoveOrSame
  /**
    * determines if a match is a move or not
    */
  private def moveOrSame(wordMatches: Set[WordMatch]): Set[(WordMatch, MoveOrSame)] = {
    val leftOrdered = wordMatches.toSeq.sortBy(_.left.from.i)
    val rightStarts = leftOrdered.map(_.right.from.i)
    val lis = LongestIncreasingSubsequence.apply(rightStarts.toArray).asScala.toSet
    val sames = wordMatches.filter(m => lis.contains(m.right.from.i))
    val moved = wordMatches -- sames
    sames.map((_, S)) ++ moved.map((_, M))
  }

  private def replaceSamesWithMoves(leftEdits: Seq[DirectCharEdit], rightEdits: Seq[DirectCharEdit], leftLine: String,
                                    rightLine: String, leftLineIdx: LineIdx, rightLineIdx: LineIdx): (Seq[CharEdit], Seq[CharEdit]) = {
    val leftSame = leftEdits.filter(_.editType ==== CharsSame)
    val rightSame = rightEdits.filter(_.editType ==== CharsSame)
    leftSame.zip(rightSame).map { case (left, right) =>
      assert(left.text(leftLine) ==== right.text(rightLine))
      val rightSelection = Selection.create(rightLine, rightLineIdx, right.from, right.to).getAssert("")
      val replacedLeft = CharEdit(left.from, left.to, CharsMoved(rightSelection, leftEdits))

      val leftSelection = Selection.create(leftLine, leftLineIdx, left.from, left.to).getAssert("")
      val replacedRight = CharEdit(right.from, right.to, CharsMoved(leftSelection, rightEdits))
      (replacedLeft, replacedRight)
    }.unzip
  }

  def calc(left: Lines, right: Lines, wordAlignment: WordAlignment, lineAlignment: LineAlignment): CharHighlight = {
    val (leftHighlight, rightHighlight) = {
      lineAlignment.matches.map { m =>

        val differ = new DiffMatchPatch()

        val wordMatchesInEitherLine = wordAlignment.matches.map { wm =>
          val matchSide = (wm.left.lineIdx ==== m.leftLineIdx, wm.right.lineIdx ==== m.rightLineIdx) match {
            case (true, true) => Some(BothMatch)
            case (true, false) => Some(LeftMatch)
            case (false, true) => Some(RightMatch)
            case (false, false) => None
          }
          (wm, matchSide)
        }.collect { case (wm, Some(side)) => (wm, side) }

        val leftLine = left.get(m.leftLineIdx).getOrElse(fail(s"Bug in code: ${m.leftLineIdx} is out of bounds"))
        val rightLine = right.get(m.rightLineIdx).getOrElse(fail(s"Bug in code: ${m.leftLineIdx} is out of bounds"))

        val wordMatchesInBothLine = wordMatchesInEitherLine.filter(_._2 ==== BothMatch).map(_._1)
        val (leftEdits, rightEdits) = moveOrSame(wordMatchesInBothLine).map { case (wm, moveOrSame) =>
          val left = wm.left.toText
          val right = wm.right.toText
          val inWordDiff = differ.diffMain(left, right)
          differ.diffCleanupSemantic(inWordDiff)

          val leftDiffs = inWordDiff.asScala.filter(d => d.operation ==== Operation.DELETE || d.operation ==== Operation.EQUAL)
          val rightDiffs = inWordDiff.asScala.filter(d => d.operation ==== Operation.INSERT || d.operation ==== Operation.EQUAL)
          val leftEdits = toPositions(wm.left, leftDiffs)
          val rightEdits = toPositions(wm.right, rightDiffs)
          if (moveOrSame ==== M) {
            replaceSamesWithMoves(leftEdits, rightEdits, leftLine, rightLine, m.leftLineIdx, m.rightLineIdx)
          } else {
            (leftEdits.map(_.toCharEdit), rightEdits.map(_.toCharEdit))
          }
        }.unzip

        val inserts = wordsWithoutMatch(rightLine, wordMatchesInEitherLine.map(_._1.right)).map(range => CharEdit(CharIdxInLine(range.startIncl), CharIdxInLine(range.endExcl), CharsInserted))
        val deletes = wordsWithoutMatch(leftLine, wordMatchesInEitherLine.map(_._1.left)).map(range => CharEdit(CharIdxInLine(range.startIncl), CharIdxInLine(range.endExcl), CharsDeleted))
        (m.leftLineIdx -> (leftEdits.flatten ++ deletes), m.rightLineIdx -> (rightEdits.flatten ++ inserts))
      }.unzip
    }

    val (leftCrossLineMoves, rightCrossLineMoves) = wordAlignment.matches.flatMap { wordMatch =>
      val bothMatch = lineAlignment.matches.filter(l => l.leftLineIdx ==== wordMatch.left.lineIdx && (l.rightLineIdx ==== wordMatch.right.lineIdx))
      (if (bothMatch.isEmpty) {
        val left = wordMatch.left.lineIdx -> Set(CharEdit(wordMatch.left.from, wordMatch.left.toExcl, CharsMoved(wordMatch.right, Traversable.empty)))
        val right = wordMatch.right.lineIdx -> Set(CharEdit(wordMatch.right.from, wordMatch.right.toExcl, CharsMoved(wordMatch.left, Traversable.empty)))
        Some((left, right))
      } else {
        None
      }).toList
    }.unzip
    CharHighlight(merge(leftHighlight, leftCrossLineMoves), merge(rightHighlight, rightCrossLineMoves))
  }
}
