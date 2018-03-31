package com.kristofszilagyi.sedito.common

import org.bitbucket.cowwoc.diffmatchpatch.DiffMatchPatch
import org.bitbucket.cowwoc.diffmatchpatch.DiffMatchPatch.Operation
import TypeSafeEqualsOps._
import scala.collection.JavaConverters._

final case class CharEdit(from: CharIdxInLine, to: CharIdxInLine, editType: EditType)

final case class CharHighlight(left: Map[LineIdx, Traversable[CharEdit]], right: Map[LineIdx, Traversable[CharEdit]])

object CharHighlightCalculator {
  def calc(left: Lines, right: Lines, wordAlignment: WordAlignment, lineAlignment: LineAlignment): CharHighlight = {
    val (leftHighlight, rightHighlight) = {
      lineAlignment.matches.map { m =>
//        val leftLine = left.get(m.leftLineIdx).getOrElse(fail(s"Bug in code: ${m.leftLineIdx} is out of bounds"))
//        val rightLine = right.get(m.rightLineIdx).getOrElse(fail(s"Bug in code: ${m.leftLineIdx} is out of bounds"))
        trait MatchSide
        case object LeftMatch extends MatchSide
        case object RightMatch extends MatchSide
        case object BothMatch extends MatchSide
        val differ = new DiffMatchPatch()

        val wordMatchesInThisLine = wordAlignment.matches.map { wm =>
          val matchSide = (wm.left.lineIdx ==== m.leftLineIdx, wm.right.lineIdx ==== m.rightLineIdx) match {
            case (true, true) => Some(BothMatch)
            case (true, false) => Some(LeftMatch)
            case (false, true) => Some(RightMatch)
            case (false, false) => None
          }
          (wm, matchSide)
        }.collect { case (wm, Some(side)) => (wm, side) }
        val (leftEdits, rightEdits) = wordMatchesInThisLine.map { case (wm, _) =>
          val left = wm.left.toText
          val rigth = wm.right.toText
          val inWordDiff = differ.diffMain(left, rigth)
          differ.diffCleanupSemantic(inWordDiff)

          val leftDiffs = inWordDiff.asScala.filter(d => d.operation ==== Operation.DELETE || d.operation ==== Operation.EQUAL)
          val rightDiffs = inWordDiff.asScala.filter(d => d.operation ==== Operation.INSERT || d.operation ==== Operation.EQUAL)

          def toPositions(diffs: Seq[DiffMatchPatch.Diff]) = {
            diffs.foldLeft(Seq.empty[CharEdit]) { case (result, diff) =>
              val op = diff.operation
              val len = diff.text.length
              val lastPos = result.lastOption.map(_.to).getOrElse(CharIdxInLine(0))
              val to = lastPos + len
              result :+ CharEdit(from = lastPos, to = to, editType = EditType.from(op))
            }
          }

          //        def addMoves(edits: Seq[PosEdits]) = {
          ////          matchSide match {
          ////            case LeftMatch =>
          ////            case RightMatch =>
          ////            case BothMatch =>
          ////          }
          //        }


          (toPositions(leftDiffs), toPositions(rightDiffs))
        }.unzip
        (m.leftLineIdx -> leftEdits.flatten, m.rightLineIdx -> rightEdits.flatten)
      }.unzip
    }
    CharHighlight(leftHighlight.toMap, rightHighlight.toMap)
  }
}
