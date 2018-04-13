package com.kristofszilagyi.sedito.gui

import java.util
import java.util.Collections

import com.kristofszilagyi.sedito.common.AssertionEx.fail
import com.kristofszilagyi.sedito.common.Warts.{DefaultArguments, discard}
import com.kristofszilagyi.sedito.common._
import com.kristofszilagyi.sedito.gui.PaddableEditor._
import javafx.scene.control.Label
import javafx.scene.text.TextFlow
import javafx.stage.Popup
import org.fxmisc.richtext.model.TwoDimensional.Bias
import org.fxmisc.richtext.model.{SegmentOps, SimpleEditableStyledDocument}
import org.fxmisc.richtext.{GenericStyledArea, LineNumberFactory, StyledTextArea}
import scala.collection.JavaConverters._


@SuppressWarnings(Array(DefaultArguments))
class SCodeArea extends GenericStyledArea[util.Collection[String], String, util.Collection[String]](
  Collections.emptyList[String],
  (paragraph: TextFlow, styleClasses: util.Collection[String]) => {
    discard(paragraph.getStyleClass.addAll(styleClasses))
    //paragraph.setStyle("-fx-padding: 0 0 100 0;")
  },
  Collections.emptyList[String],
  new SimpleEditableStyledDocument (
    Collections.emptyList[String](), Collections.emptyList[String]()
  ),
  SegmentOps.styledTextOps(),
  false,
  seg => StyledTextArea.createStyledTextNode(seg, (text, styleClasses: util.Collection[String]) => {
    discard(text.getStyleClass.addAll(styleClasses))
  }),
) {
}


object PaddableEditor {
  def test(): PaddableEditor = {
    val editor = new PaddableEditor
    editor.appendText("that's a lot\n of text\n so many\ntext")
    editor
  }

  final case class LineCssClass(s: String) {
    def toChar: CharCssClass = CharCssClass(s"${s}_char")
  }
  final case class CharCssClass(s: String)

  private def getLineCssClass(editType: Option[LineEditType]) = {
    (editType map {
      case _: LineMoved => LineCssClass("moved")
      case LineInserted => LineCssClass("inserted")
      case LineDeleted => LineCssClass("deleted")
      case LineSame => LineCssClass("same")
    }).getOrElse(LineCssClass("white"))
  }

  private def getCharCssClass(editType: Option[CharEditType]) = {
    (editType map {
      case CharsInserted => CharCssClass("inserted_char")
      case CharsDeleted => CharCssClass("deleted_char")
      case CharsSame => CharCssClass("same_char")
      case _: CharsMoved => CharCssClass("moved_char")
    }).getOrElse(CharCssClass("white_char"))
  }
}

final case class LineEdits(line: LineEditType, charEdits: Traversable[CharEdit])

final class PaddableEditor extends SCodeArea {
  this.setUseInitialStyleForInsertion(true)

  setParagraphGraphicFactory(LineNumberFactory.get(this))

  import java.time.Duration

  import org.fxmisc.richtext.event.MouseOverTextEvent

  @SuppressWarnings(Array(Warts.Var))
  private var editTypes = Map.empty[LineIdx, LineEdits]

  @SuppressWarnings(Array(Warts.Var))
  private var paddings = Map.empty[LineIdx, NumberOfLinesPadding]

  @SuppressWarnings(Array(Warts.Var))
  private var otherEditor: Option[PaddableEditor] = None

  @SuppressWarnings(Array(Warts.Var))
  private var highlightedLines: Traversable[LineIdx] = Traversable.empty

  @SuppressWarnings(Array(Warts.Var))
  private var highlightedChars: Traversable[Selection] = Traversable.empty

  def reset(): Unit = {
    editTypes = Map.empty
    paddings = Map.empty
    highlightedLines = Traversable.empty
    highlightedChars = Traversable.empty
    clear()
  }
  private val popup = new Popup
  private val popupMsg = new Label
  popupMsg.setStyle("-fx-background-color: black;" + "-fx-text-fill: white;" + "-fx-padding: 5;")
  discard(popup.getContent.add(popupMsg))
  setMouseOverTextDelay(Duration.ofMillis(1))
  addEventHandler(MouseOverTextEvent.MOUSE_OVER_TEXT_BEGIN, (e: MouseOverTextEvent) => {

    val chIdx = e.getCharacterIndex
    val posOnScreen = e.getScreenPosition
    val posInText = offsetToPosition(chIdx, Bias.Forward)
    editTypes.get(LineIdx(posInText.getMajor)) match {
      case Some(edit) =>
        val cursorPosInLine = CharIdxInLine(posInText.getMinor)
        val moveUnderCursor = edit.charEdits.collect {
          case charEdit @ CharEdit(_, _, CharsMoved(fromTo, _)) if charEdit.contains(cursorPosInLine) =>
            fromTo
        }
        assert(moveUnderCursor.size <= 1, s"$moveUnderCursor")
        moveUnderCursor.headOption match {
          case Some(fromTo) =>
            popupMsg.setText(s"Moved from/to line ${fromTo.lineIdx.i + 1}")
            popup.show(this, posOnScreen.getX, posOnScreen.getY + 10)
            otherEditor.foreach(_.highlightChar(fromTo))
          case None =>
            edit.line match {
              case LineMoved(from) =>
                popupMsg.setText(s"Moved from/to line ${from.i + 1}")
                popup.show(this, posOnScreen.getX, posOnScreen.getY + 10)
                otherEditor.foreach(_.highlightLine(from))
              case LineInserted | LineDeleted | LineSame=>
            }

        }

      case None =>
    }
  })

  discard(plainTextChanges.subscribe(_ => applyAllPadding()))

  addEventHandler(MouseOverTextEvent.MOUSE_OVER_TEXT_END, (e: MouseOverTextEvent) => {
    popup.hide()
    otherEditor.foreach(_.resetHighlighting())
  })

  private def applyPaddingCss(line: LineIdx, padding: NumberOfLinesPadding): Unit = {
    val height = 16
    this.setParagraphBoxStyle(line.i, s"-fx-padding: ${height * padding.i} 0 0 0;")
  }

  def setLinePadding(line: LineIdx, padding: NumberOfLinesPadding): Unit = {
    paddings += line -> padding
    applyPaddingCss(line, padding)
  }

  def applyAllPadding(): Unit = {
    paddings.foreach((applyPaddingCss _).tupled)
  }

  private def applyLineTypeCss(lineIdx: LineIdx, editType: Option[LineEdits]): Unit = {
    //setParagraphStyle(lineIdx.i, List("same").asJava)
    setParagraphStyle(lineIdx.i, List(getLineCssClass(editType.map(_.line)).s).asJava)
    editType.toList.flatMap(_.charEdits).foreach{ edit =>
      edit.editType match {
        case tpe: ApplicableCharEditType =>
          applyCharCss(lineIdx, edit.from, edit.to, tpe)
        case tpe @ CharsMoved(_, edits) =>
          applyCharCss(lineIdx, edit.from, edit.to, tpe)
          edits.foreach{ editInMove =>
            applyCharCss(lineIdx, editInMove.from, editInMove.to, editInMove.editType)
          }
      }
    }
    paddings.get(lineIdx).foreach(applyPaddingCss(lineIdx, _))
  }

  def setLineType(lineIdx: LineIdx, editType: LineEditType): Unit = {
    val current = editTypes.get(lineIdx)
    val newEdit = current match {
      case Some(edit) => edit.copy(line = editType)
      case None => LineEdits(editType, Traversable.empty)
    }

    editTypes += lineIdx -> newEdit
    applyLineTypeCss(lineIdx, Some(newEdit))
  }

  private def applyCharCss(lineIdx: LineIdx, from: CharIdxInLine, to: CharIdxInLine, editType: CharEditType): Unit = {
    setStyle(lineIdx.i, from.i, to.i, List(getCharCssClass(Some(editType)).s).asJava)
  }

  def setCharEdit(lineIdx: LineIdx, from: CharIdxInLine, to: CharIdxInLine, editType: CharEditType): Unit = {
    val current = editTypes.get(lineIdx)
    val newEdit = current match {
      case Some(edit) => edit.copy(charEdits = edit.charEdits ++ Traversable(CharEdit(from, to, editType)))
      case None => fail(s"Bug in code: cannot have new char edits without line edit. $lineIdx")
    }
    editTypes += lineIdx -> newEdit
    applyLineTypeCss(lineIdx, Some(newEdit))
  }

  def highlightLine(lineIdx: LineIdx): Unit = {
    highlightedLines ++= Traversable(lineIdx)
    setParagraphStyle(lineIdx.i, List("highlighted").asJava)
  }

  def highlightChar(selection: Selection): Unit = {
    highlightedChars ++= Traversable(selection)
    setStyle(selection.lineIdx.i, selection.from.i, selection.toExcl.i, List("highlighted_char").asJava)
  }

  def resetHighlighting(): Unit = {
    highlightedLines.foreach { line =>
      applyLineTypeCss(line, editTypes.get(line))
    }
    highlightedLines = Traversable.empty

    highlightedChars.foreach { selection =>
      val line = selection.lineIdx
      applyLineTypeCss(line, editTypes.get(line))
    }
    highlightedChars = Traversable.empty
  }

  def setOther(other: PaddableEditor): Unit = {
    otherEditor = Some(other)
  }

}
