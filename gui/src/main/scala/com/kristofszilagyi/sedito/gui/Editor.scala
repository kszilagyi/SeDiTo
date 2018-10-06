package com.kristofszilagyi.sedito.gui

import java.time.Duration
import java.util

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
import org.fxmisc.richtext.event.MouseOverTextEvent
import org.fxmisc.richtext.model.TwoDimensional.Bias
import org.fxmisc.richtext.model.{StyleSpans, StyleSpansBuilder}
import org.fxmisc.richtext.{CodeArea, GenericStyledArea}

import scala.collection.JavaConverters._
import scala.compat.java8.OptionConverters.RichOptionalGeneric

final case class MatchInfo(selection: Selection, probability: Option[Double])

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

  private val sameCharClass = CharCssClass("same_char")
  private val insertedCharClass = CharCssClass("inserted_char")
  private val deletedCharClass = CharCssClass("deleted_char")
  private val movedCharClass = CharCssClass("moved_char")

  private def getCharCssClass(editType: CharEditType, lineEditType: LineEditType) = {
    editType match {
      case CharsInserted => insertedCharClass
      case CharsDeleted => deletedCharClass
      case CharsSame =>
        lineEditType match {
          case LineMoved(_) => CharCssClass("same_char_in_moved_line")
          case LineInserted => fail("Char is same but line is inserted")
          case LineDeleted => fail("Char is same but line is deleted")
          case LineSame => sameCharClass
        }
      case _: CharsMoved => movedCharClass
    }
  }

  private final case class Span(cssClass: CharCssClass, length: Int)
  private def calculateMovedSpans(lineEditType: LineEditType, move: CharEdit, minorEdits: Seq[ActualCharEdit]) = {
    val moveStyle = getCharCssClass(move.editType, lineEditType)
    val (spans, finalPos) = minorEdits.foldLeft((Seq.empty[Span], move.from)) { case ((acc, lastPos), edit) =>
      val newAcc = acc :+ Span(moveStyle, edit.from.i - lastPos.i) :+
        Span(getCharCssClass(edit.editType, lineEditType), edit.length)
      (newAcc, edit.to)
    }
    spans :+ Span(moveStyle, move.to.i - finalPos.i)
  }

  private def charClassForLineEdit(lineEditType: LineEditType) = {
    lineEditType match {
      case LineMoved(_) => movedCharClass
      case LineInserted => insertedCharClass
      case LineDeleted => deletedCharClass
      case LineSame => sameCharClass
    }
  }

  private def toStylesSpans(highlight: Map[LineIdx, LineEdits], lines: IndexedSeq[String]): StyleSpans[util.Collection[String]] = {
    val builder = new StyleSpansBuilder[util.Collection[String]]
    highlight.toSeq.sortBy(_._1).foreach{ case (lineIdx, edits) =>
      @SuppressWarnings(Array(Warts.Var))
      var nextIdxInLine = 0
      val lineClass = charClassForLineEdit(edits.line)
      edits.charEdits.toSeq.sortBy(_.from).foreach { edit =>

        if (edit.from.i > nextIdxInLine) {
          discard(builder.add(List(lineClass.s).asJava, edit.from.i - nextIdxInLine))
        }

        val spans = edit.editType match {
          case tpe: ApplicableCharEditType =>
            Seq(Span(getCharCssClass(tpe, edits.line), edit.length))
          case CharsMoved(_, minorEdits) =>
            calculateMovedSpans(edits.line, edit, minorEdits.toSeq.sortBy(_.from))
        }
        spans.foreach { span =>
          discard(builder.add(List(span.cssClass.s).asJava, span.length))
        }
        nextIdxInLine = edit.to.i
      }
      val maybeLine = lines.lift(lineIdx.i)
      maybeLine.foreach { line =>
        discard(builder.add(List(lineClass.s).asJava, line.length - nextIdxInLine))
      }
    }
    builder.create()
  }

}

final case class LineEdits(line: LineEditType, charEdits: Traversable[CharEdit])

final class Editor extends CodeArea {
  this.setUseInitialStyleForInsertion(true)

  private val lineNumberFactory = new CacheLineNumberFactory(this)
  setParagraphGraphicFactory(lineNumberFactory)

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

  @SuppressWarnings(Array(Warts.Var))
  var wordAlignmentByLine: Map[LineIdx, Traversable[MatchInfo]] = Map.empty

  def selectedForMatch(): Option[Selection] = _selectedForMatch

  def reset(): Unit = {
    editTypes = Map.empty
    highlightedLines = Traversable.empty
    highlightedChars = Traversable.empty
    lineNumberFactory.reset()
    clear()
  }
  private val popup = new Popup
  private val popupMsg = new Label
  popupMsg.setStyle("-fx-background-color: black;" + "-fx-text-fill: white;" + "-fx-padding: 5;")
  discard(popup.getContent.add(popupMsg))

  private val popupDebug = new Popup
  private val popupDebugMsg = new Label
  popupDebugMsg.setStyle("-fx-background-color: black;" + "-fx-text-fill: white;" + "-fx-padding: 5;")
  discard(popupDebug.getContent.add(popupDebugMsg))

  setMouseOverTextDelay(Duration.ofMillis(1))
  addEventHandler(MouseOverTextEvent.MOUSE_OVER_TEXT_BEGIN, (e: MouseOverTextEvent) => {

    val chIdx = e.getCharacterIndex
    val posOnScreen = e.getScreenPosition
    val posInText = offsetToPosition(chIdx, Bias.Forward)
    val line = LineIdx(posInText.getMajor)
    val cursorPosInLine = CharIdxInLine(posInText.getMinor)
    editTypes.get(line).foreach { edit =>
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
    }

    wordAlignmentByLine.get(line).foreach{ matches =>
      val maybeMatchUnderCursor = matches.find(m => m.selection.from <= cursorPosInLine && cursorPosInLine < m.selection.toExcl)
      val probability = maybeMatchUnderCursor.flatMap(_.probability).getOrElse(Double.NaN)
      popupDebugMsg.setText(f"$probability%.2f")
      popupDebug.show(this, posOnScreen.getX, posOnScreen.getY + 30)
    }
  })

  addEventHandler(MouseOverTextEvent.MOUSE_OVER_TEXT_END, (e: MouseOverTextEvent) => {
    popup.hide()
    popupDebug.hide()
    otherEditor.foreach(_.resetHighlighting())
  })

  private def applyLineTypeCss(lineIdx: LineIdx, editType: Option[LineEdits]): Unit = {
    val lineCssClass = getLineCssClass(editType.map(_.line)).s
    setParagraphStyle(lineIdx.i, List(lineCssClass).asJava)

    {
      val lineNoStyle = lineNumberFactory.apply(lineIdx.i).getStyleClass
      lineNoStyle.clear()
      discard(lineNoStyle.add(lineCssClass))
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

  def applyCharEdits(): Unit = {
    setStyleSpans(0, toStylesSpans(editTypes, this.getText().linesWithSeparators.toIndexedSeq))
  }

  def setCharEdit(lineIdx: LineIdx, from: CharIdxInLine, to: CharIdxInLine, editType: CharEditType): Unit = {
    val current = editTypes.get(lineIdx)
    val newEdit = current match {
      case Some(edit) => edit.copy(charEdits = edit.charEdits ++ Traversable(CharEdit(from, to, editType)))
      case None => fail(s"Bug in code: cannot have new char edits without line edit. $lineIdx")
    }
    editTypes += lineIdx -> newEdit
  }

  private def highlightLine(lineIdx: LineIdx): Unit = {
    highlightedLines ++= Traversable(lineIdx)
    setParagraphStyle(lineIdx.i, List("highlighted").asJava)
  }

  private def highlightChar(selection: Selection): Unit = {
    highlightedChars ++= Traversable(selection)
    setStyle(selection.lineIdx.i, selection.from.i, selection.toExcl.i, List("highlighted_char").asJava)
  }

  private def resetHighlighting(): Unit = {
    highlightedLines.foreach { line =>
      applyLineTypeCss(line, editTypes.get(line))
    }
    highlightedLines = Traversable.empty

    //todo this doesn't work anymore as applyLineTypeCss doesn't change the char css anymore
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
    val newSelection = Selection.fromAbsolute(indexRange.getStart, indexRange.getEnd, FullText(getText)).getAssert

    if (newSelection.empty) _selectedForMatch = None
    else _selectedForMatch = Some(newSelection)
  }

  def lineIndicesOnScreen(): LineRange = {
    // this 2 lines trigger recalculation so from the next call the results are constant in this tick
    // (bug in the framework)
    //they are equal but has a difference memory address
    allParToVisibleParIndex(firstVisibleParToAllParIndex()).asScala.foreach(getVisibleParagraphBoundsOnScreen(_))
    allParToVisibleParIndex(lastVisibleParToAllParIndex()).asScala.foreach(getVisibleParagraphBoundsOnScreen(_))

    LineRange(LineIdx(firstVisibleParToAllParIndex()), LineIdx(lastVisibleParToAllParIndex()) + 1)
  }

  private def getBounds(line: LineIdx) = {
    val f = classOf[GenericStyledArea[_, _, _]].getDeclaredField("virtualFlow")
    f.setAccessible(true)
    @SuppressWarnings(Array(Warts.AsInstanceOf))
    val virtualFlow = f.get(this).asInstanceOf[VirtualFlow[_, Cell[_, Node]]]

    allParToVisibleParIndex(line.i).asScala.map { i =>
      val node = virtualFlow.visibleCells.get(i).getNode
      val uncropped = node.localToScreen(node.getBoundsInLocal)
      val cropped = getVisibleParagraphBoundsOnScreen(i)
      new BoundingBox(cropped.getMinX, uncropped.getMinY, cropped.getWidth, uncropped.getHeight)
    }
  }
  def boundsInLocal(line: LineIdx, convertToLocal: Bounds => Bounds): Option[Bounds] = {
      def calcOnScreenBounds() = {
        // the reflective call is necessary because the stock method(getVisibleParagraphBoundsOnScreen)
        // is cropping the original rectangle to the edges of the screen.

        if(getParagraphs.size() !=== line.i) getBounds(line)
        else {
          val bounds = getBounds(line - 1)
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
