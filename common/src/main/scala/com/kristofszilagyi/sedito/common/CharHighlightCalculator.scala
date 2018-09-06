package com.kristofszilagyi.sedito.common

import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.utils.MapOps.RichMap
import org.bitbucket.cowwoc.diffmatchpatch.DiffMatchPatch
import org.bitbucket.cowwoc.diffmatchpatch.DiffMatchPatch.Operation

import scala.collection.JavaConverters._

/**
  * This one can contain moves too but DirectCharEdit can't
  */
final case class CharEdit(from: CharIdxInLine, to: CharIdxInLine, editType: CharEditType) {
  def text(line: String): String = {
    line.substring(from.i, to.i)
  }

  def contains(pos: CharIdxInLine): Boolean = {
    from <= pos && pos < to
  }

  def length: Int = {
    to.i - from.i
  }
}


final case class DirectCharEdit(from: CharIdxInLine, to: CharIdxInLine, editType: ApplicableCharEditType) {
  def text(line: String): String = {
    line.substring(from.i, to.i)
  }
  def toCharEdit: CharEdit = CharEdit(from, to, editType)
}

object ActualCharEdit {
  @SuppressWarnings(Array(Warts.Overloading))
  def unapply(edit: DirectCharEdit): Option[ActualCharEdit] = {
    edit.editType match {
      case t: ActualCharEditType =>
        Some(ActualCharEdit(edit.from, edit.to, t))
      case CharsSame => None
    }
  }
}

final case class ActualCharEdit(from: CharIdxInLine, to: CharIdxInLine, editType: ActualCharEditType) {
  def text(line: String): String = {
    line.substring(from.i, to.i)
  }
  def toCharEdit: CharEdit = CharEdit(from, to, editType)

  def length: Int = to.i - from.i
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
      val lastPos = result.lastOption.map(_.to).getOrElse(baseline.from)
      val to = lastPos + len
      result :+ DirectCharEdit(from = lastPos, to = to, editType = CharEditType.from(op))
    }
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

  private def replaceSamesWithMoves(wm: WordMatch, leftEdits: Seq[DirectCharEdit], rightEdits: Seq[DirectCharEdit]): (Seq[CharEdit], Seq[CharEdit]) = {

    def keepActualEdits(edits: Seq[DirectCharEdit]): Seq[ActualCharEdit] = {
      edits.collect{
        case ActualCharEdit(a) => a
      }
    }
    val leftSame = leftEdits.filter(_.editType ==== CharsSame)
    val rightSame = rightEdits.filter(_.editType ==== CharsSame)
    //this must be wrong TODO!
    leftSame.zip(rightSame).map { _ =>
      val replacedLeft = CharEdit(wm.left.from, wm.left.toExcl, CharsMoved(wm.right, keepActualEdits(leftEdits)))

      val replacedRight = CharEdit(wm.right.from, wm.right.toExcl, CharsMoved(wm.left, keepActualEdits(rightEdits)))
      (replacedLeft, replacedRight)
    }.unzip
  }

  private def removeOverlaps(from: Traversable[CharEdit], overlapWith: Traversable[CharEdit]) = {
    from.filterNot(fromItem => overlapWith.exists(over => (over.from, over.to) ==== ((fromItem.from, fromItem.to))))
  }

  //todo this generates extra sames
  def calc(left: IndexedSeq[Selection], right: IndexedSeq[Selection], wordAlignment: UnambiguousWordAlignment, lineAlignment: UnambiguousLineAlignment): CharHighlight = {
    val leftByLine = left.groupBy(_.lineIdx).mapValuesNow(_.toSet)
    val rightByLine = right.groupBy(_.lineIdx).mapValuesNow(_.toSet)
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
            replaceSamesWithMoves(wm, leftEdits, rightEdits)
          } else {
            (leftEdits.map(_.toCharEdit), rightEdits.map(_.toCharEdit))
          }
        }.unzip

        val deletes = (leftByLine.getOrElse(m.leftLineIdx, Set.empty) -- wordMatchesInEitherLine.map(_._1.left)).map(range => CharEdit(range.from, range.toExcl, CharsDeleted))
        val inserts = (rightByLine.getOrElse(m.rightLineIdx, Set.empty) -- wordMatchesInEitherLine.map(_._1.right)).map(range => CharEdit(range.from, range.toExcl, CharsInserted))
        val leftEditsWithoutDeletes = removeOverlaps(leftEdits.flatten, deletes)
        val rightEditWithoutInserts = removeOverlaps(rightEdits.flatten, inserts)
        (m.leftLineIdx -> (leftEditsWithoutDeletes.toSet ++ deletes), m.rightLineIdx -> (rightEditWithoutInserts.toSet ++ inserts))
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
