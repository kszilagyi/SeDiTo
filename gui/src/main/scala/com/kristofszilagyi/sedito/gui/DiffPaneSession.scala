package com.kristofszilagyi.sedito.gui

import java.nio.file.Path

import com.kristofszilagyi.sedito.common.TraversableOps.RichTraversable
import com.kristofszilagyi.sedito.common._
import TypeSafeEqualsOps._
import scala.collection.immutable.TreeMap

object DiffPaneSession {
  def empty: DiffPaneSession = {
    new DiffPaneSession(UnambiguousWordAlignment(Set.empty), Traversable.empty, notMovedLines = TreeMap.empty,
      maybeLeftPath = None, maybeRightPath = None, Traversable.empty)
  }

  private def changedLines(highlight: Map[LineIdx, Traversable[CharEdit]]) = {
    highlight.filter { case (_, edits) =>
      edits.exists(_.editType !=== CharsSame)
    }
  }

  private def allChanges(eqPoints: Traversable[LineChangePoint], highlight: CharHighlight, lineAlignment: UnambiguousLineAlignment) = {
    val laLeftLookup = lineAlignment.matches.groupBy(_.leftLineIdx)
    val laRightLookup = lineAlignment.matches.groupBy(_.rightLineIdx)
    val lineMatchesWithLeftCharChanges = changedLines(highlight.left).flatMap{case (lineIdx, _) => laLeftLookup.getOrElse(lineIdx, Set.empty).forceToOption}
    val lineMatchesWithRightCharChanges = changedLines(highlight.right).flatMap{case (lineIdx, _) => laRightLookup.getOrElse(lineIdx, Set.empty).forceToOption}

    val allChanges = eqPoints.map(e => ChangePointStart(e.left.from, e.right.from)) ++
      lineMatchesWithLeftCharChanges.map(m => ChangePointStart(m.leftLineIdx, m.rightLineIdx)) ++
      lineMatchesWithRightCharChanges.map(m => ChangePointStart(m.leftLineIdx, m.rightLineIdx))

    val withoutSameFromLeft = allChanges.groupBy(_.left).values.flatMap(_.headOption)
    val withoutSameFromBoth = withoutSameFromLeft.groupBy(_.right).values.flatMap(_.headOption)
    withoutSameFromBoth
  }

  def create(left: FullText, right: FullText, newMaybeLeftPath: Option[Path], newMaybeRightPath: Option[Path],
             newWordAlignment: UnambiguousWordAlignment): (DiffPaneSession, EditorSession, EditorSession) = {
    val leftLines = Lines(left.s.linesIterator.toIndexedSeq)
    val rightLines = Lines(right.s.linesIterator.toIndexedSeq)

    val lineAlignment = WhiteSpaceAligner.align(leftLines, rightLines, LineAligner.align(newWordAlignment))

    val deleted = leftLines.l.indices.map(LineIdx.apply).filterNot(l => lineAlignment.matches.map(_.leftLineIdx).contains(l))
    val inserted = rightLines.l.indices.map(LineIdx.apply).filterNot(l => lineAlignment.matches.map(_.rightLineIdx).contains(l))
    val partitioned = lineAlignment.partition
    val moved = partitioned.moved
    val movedLeft = moved.map(BiasedLineMatch.left)
    val movedRight = moved.map(BiasedLineMatch.right)
    val notMovedLines = TreeMap(partitioned.notMoved.flatMap(LineMatch.unapply).toSeq: _*)
    val notMovedLeft = partitioned.notMoved.map(_.leftLineIdx)
    val notMovedRight = partitioned.notMoved.map(_.rightLineIdx)

    val eqPoints = LineChangePointCalculator.calc(partitioned.notMoved, moved, leftLineCount = leftLines.l.size,
      rightLineCount = rightLines.l.size)

    val leftWords = Wordizer.toWordIndices(left.s)
    val rightWords = Wordizer.toWordIndices(right.s)
    val highlight = CharHighlightCalculator.calc(leftWords, rightWords, newWordAlignment, lineAlignment)

    val leftGroupedWordAlignment = newWordAlignment.matches.map(m => MatchInfo(m.left, m.probability)).groupBy(_.selection.lineIdx)
    val rightGroupedWordAlignment = newWordAlignment.matches.map(m => MatchInfo(m.right, m.probability)).groupBy(_.selection.lineIdx)

    val leftSession = EditorSession.create(left, leftGroupedWordAlignment, deleted, LineDeleted, movedLeft, notMovedLeft, highlight.left)
    val rightSession = EditorSession.create(right, rightGroupedWordAlignment, inserted, LineInserted, movedRight, notMovedRight, highlight.right)

    (new DiffPaneSession(newWordAlignment, eqPoints, notMovedLines, newMaybeLeftPath, newMaybeRightPath,
      allChanges(eqPoints, highlight, lineAlignment)), leftSession, rightSession)
  }
}

private[gui] final class DiffPaneSession(val wordAlignment: UnambiguousWordAlignment,
                                         val lineChangePoints: Traversable[LineChangePoint],
                                         val notMovedLines: TreeMap[LineIdx, LineIdx],
                                         val maybeLeftPath: Option[Path],
                                         val maybeRightPath: Option[Path],
                                         allChangePoints: Traversable[ChangePointStart]) {
  val nextChangeTracker: NextChangeTracker = new NextChangeTracker(allChangePoints)
}