package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.AssertionEx.fail
import com.kristofszilagyi.sedito.common._
import com.kristofszilagyi.sedito.gui.utils.LineEndingUtils

object EditorSession {
  def empty: EditorSession = {
    new EditorSession(
      editTypes = Map.empty[LineIdx, LineEdits],
      wordAlignmentByLine = Map.empty,
      newLineType = System.lineSeparator()
    )
  }

  private final class LineTypeBuilder() {
    @SuppressWarnings(Array(Warts.Var))
    private var editTypes: Map[LineIdx, LineEdits] = Map.empty

    def setLineType(lineIdx: LineIdx, editType: LineEditType): Unit = {
      val current = editTypes.get(lineIdx)
      val newEdit = current match {
        case Some(edit) => edit.copy(line = editType)
        case None => LineEdits(editType, Vector.empty)
      }

      editTypes += lineIdx -> newEdit
    }

    def applyHighlight(highlight: Map[LineIdx, Traversable[CharEdit]]): Unit = {
      highlight.foreach { case (line, edits) =>
        edits.foreach { edit =>
          setCharEdit(line, edit.from, edit.to, edit.editType)
        }
      }
    }

    private def setCharEdit(lineIdx: LineIdx, from: CharIdxInLine, to: CharIdxInLine, editType: CharEditType): Unit = {
      val current = editTypes.get(lineIdx)
      val newEdit = current match {
        case Some(edit) => edit.copy(charEdits = edit.charEdits :+ CharEdit(from, to, editType))
        case None => fail(s"Bug in code: cannot have new char edits without line edit. $lineIdx")
      }
      editTypes += lineIdx -> newEdit
    }

    def build(): Map[LineIdx, LineEdits] = editTypes
  }



  def create(fullText: FullText, wordAlignment: Map[LineIdx, scala.Traversable[MatchInfo]], changed: IndexedSeq[LineIdx],
              changeType: LineEditType, moved: Set[BiasedLineMatch], notMoved: Set[LineIdx],
              highlight: Map[LineIdx, scala.Traversable[CharEdit]]): EditorSession = {

    val lineTypeBuilder = new LineTypeBuilder()
    changed.foreach(l => lineTypeBuilder.setLineType(l, changeType))
    moved.foreach(m => lineTypeBuilder.setLineType(m.thisSide, LineMoved(m.otherSide)))
    notMoved.foreach(l => lineTypeBuilder.setLineType(l, LineSame))

    lineTypeBuilder.applyHighlight(highlight)

    new EditorSession(lineTypeBuilder.build(), wordAlignment, LineEndingUtils.guessLineEnding(fullText))
  }
}

final class EditorSession(val editTypes: Map[LineIdx, LineEdits],
                          val wordAlignmentByLine: Map[LineIdx, Traversable[MatchInfo]], val newLineType: String) {

}