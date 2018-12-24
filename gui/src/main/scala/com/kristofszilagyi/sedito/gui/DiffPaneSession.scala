package com.kristofszilagyi.sedito.gui

import java.nio.file.Path

import com.kristofszilagyi.sedito.common._

import scala.collection.immutable.TreeMap

object DiffPaneSession {
  def empty: DiffPaneSession = {
    new DiffPaneSession(UnambiguousWordAlignment(Set.empty), Traversable.empty, notMovedLines = TreeMap.empty, maybeLeftPath = None, maybeRightPath = None)
  }

  def create(left: FullText, right: FullText, newMaybeLeftPath: Option[Path], newMaybeRightPath: Option[Path],
             newWordAlignment: UnambiguousWordAlignment): (DiffPaneSession, EditorSession, EditorSession) = {
    val leftLines = Lines(left.s.lines.toIndexedSeq)
    val rightLines = Lines(right.s.lines.toIndexedSeq)

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

    val eqPoints = InsertionPointCalculator.calc(partitioned.notMoved, moved, leftLineCount = leftLines.l.size,
      rightLineCount = rightLines.l.size)

    val leftWords = Wordizer.toWordIndices(left.s)
    val rightWords = Wordizer.toWordIndices(right.s)
    val highlight = CharHighlightCalculator.calc(leftWords, rightWords, newWordAlignment, lineAlignment)

    val leftGroupedWordAlignment = newWordAlignment.matches.map(m => MatchInfo(m.left, m.probability)).groupBy(_.selection.lineIdx)
    val rightGroupedWordAlignment = newWordAlignment.matches.map(m => MatchInfo(m.right, m.probability)).groupBy(_.selection.lineIdx)


    val leftSession = EditorSession.create(left, leftGroupedWordAlignment, deleted, LineDeleted, movedLeft, notMovedLeft, highlight.left)
    val rightSession = EditorSession.create(right, rightGroupedWordAlignment, inserted, LineInserted, movedRight, notMovedRight, highlight.right)


    (new DiffPaneSession(newWordAlignment, eqPoints, notMovedLines, newMaybeLeftPath, newMaybeRightPath), leftSession, rightSession)
  }
}

private[gui] final class DiffPaneSession(val wordAlignment: UnambiguousWordAlignment = UnambiguousWordAlignment(Set.empty),
                                         val eqPoints: Traversable[EquivalencePoint] = Traversable.empty,
                                         val notMovedLines: TreeMap[LineIdx, LineIdx] = TreeMap.empty,
                                         val maybeLeftPath: Option[Path] = None,
                                         val maybeRightPath: Option[Path] = None)


