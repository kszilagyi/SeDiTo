package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.AssertionEx.fail
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.common._
import com.kristofszilagyi.sedito.gui.Editor._
import javafx.geometry.{BoundingBox, Bounds}
import javafx.scene.Node
import javafx.scene.control.Label
import javafx.stage.Popup
import org.fxmisc.flowless.{Cell, VirtualFlow}
import org.fxmisc.richtext.model.TwoDimensional.Bias
import org.fxmisc.richtext.{CodeArea, GenericStyledArea, LineNumberFactory}

import scala.collection.JavaConverters._
import scala.compat.java8.OptionConverters.RichOptionalGeneric


object Editor {

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

  private def getCharCssClass(editType: CharEditType, lineEditType: LineEditType) = {
    editType match {
      case CharsInserted => CharCssClass("inserted_char")
      case CharsDeleted => CharCssClass("deleted_char")
      case CharsSame =>
        lineEditType match {
          case LineMoved(_) => CharCssClass("same_char_in_moved_line")
          case LineInserted => fail("Char is same but line is inserted")
          case LineDeleted => fail("Char is same but line is deleted")
          case LineSame => CharCssClass("same_char")
        }
      case _: CharsMoved => CharCssClass("moved_char")
    }
  }
}

final case class LineEdits(line: LineEditType, charEdits: Traversable[CharEdit])

final class Editor extends CodeArea {
  this.setUseInitialStyleForInsertion(true)

  setParagraphGraphicFactory(LineNumberFactory.get(this))

  import java.time.Duration

  import org.fxmisc.richtext.event.MouseOverTextEvent

  @SuppressWarnings(Array(Warts.Var))
  private var editTypes = Map.empty[LineIdx, LineEdits]

  @SuppressWarnings(Array(Warts.Var))
  private var otherEditor: Option[Editor] = None

  @SuppressWarnings(Array(Warts.Var))
  private var highlightedLines: Traversable[LineIdx] = Traversable.empty

  @SuppressWarnings(Array(Warts.Var))
  private var highlightedChars: Traversable[Selection] = Traversable.empty

  @SuppressWarnings(Array(Warts.Var))
  private var _selectedForMatch: Option[Selection] = None

  def selectedForMatch(): Option[Selection] = _selectedForMatch

  def reset(): Unit = {
    editTypes = Map.empty
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

  addEventHandler(MouseOverTextEvent.MOUSE_OVER_TEXT_END, (e: MouseOverTextEvent) => {
    popup.hide()
    otherEditor.foreach(_.resetHighlighting())
  })

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
    setStyle(lineIdx.i, from.i, to.i,
      List(
        getCharCssClass(
          editType,
          editTypes.getOrElse(lineIdx, fail(s"Missing line edit type for $lineIdx")).line
        ).s).asJava
    )
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

  def setOther(other: Editor): Unit = {
    otherEditor = Some(other)
  }

  def grabSelectionForMatch(): Unit = {
    val indexRange = getSelection
    val newSelection = WordIndexRange.create(indexRange.getStart, indexRange.getEnd, FullText(getText))
      .map(_.toSelection).getAssert

    if (newSelection.empty) _selectedForMatch = None
    else _selectedForMatch = Some(newSelection)
  }

  def lineIndicesOnScreen(): LineRange = {
    // this 2 lines trigger recalculation so from the next call the results are constant in this tick
    // (bug in the framework)
    allParToVisibleParIndex(firstVisibleParToAllParIndex()).asScala.foreach(getVisibleParagraphBoundsOnScreen(_))
    allParToVisibleParIndex(lastVisibleParToAllParIndex()).asScala.foreach(getVisibleParagraphBoundsOnScreen(_))

    LineRange(LineIdx(firstVisibleParToAllParIndex()), LineIdx(lastVisibleParToAllParIndex()) + 1)
  }

  def boundsInLocal(line: LineIdx, convertToLocal: Bounds => Bounds): Option[Bounds] = {
      def calcOnScreenBounds() = {
        // the reflective call is necessary because the stock method(getVisibleParagraphBoundsOnScreen)
        // is cropping the original rectangle to the edges of the screen.
        val f = classOf[GenericStyledArea[_, _, _]].getDeclaredField("virtualFlow")
        f.setAccessible(true)
        @SuppressWarnings(Array(Warts.AsInstanceOf))
        val virtualFlow = f.get(this).asInstanceOf[VirtualFlow[_, Cell[_, Node]]]
        if(getParagraphs.size() !=== line.i) allParToVisibleParIndex(line.i).map[Bounds] { i =>
          val node = virtualFlow.visibleCells.get(i).getNode
          node.localToScreen(node.getBoundsInLocal)
        }.asScala else {
          val bounds = allParToVisibleParIndex(line.i - 1).map[Bounds] { i =>
            val node = virtualFlow.visibleCells.get(i).getNode
            node.localToScreen(node.getBoundsInLocal)
          }.asScala
          bounds.map{ b =>
            new BoundingBox(b.getMinX, b.getMaxY, b.getWidth, 0)
          }
        }
      }
      val potentiallyBadResult = calcOnScreenBounds()
      discard(potentiallyBadResult) // bug in the framework
      val onScreenBounds = calcOnScreenBounds()
      onScreenBounds.map(convertToLocal)
  }

}
