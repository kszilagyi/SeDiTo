package com.kristofszilagyi.sedito.gui

import java.time.Duration
import java.util

import com.kristofszilagyi.sedito.common.AssertionEx.fail
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.common._
import com.kristofszilagyi.sedito.gui.Editor._
import com.kristofszilagyi.sedito.gui.utils.LineEndingUtils
import javafx.geometry.{BoundingBox, Bounds}
import javafx.scene.Node
import javafx.scene.control.Label
import javafx.stage.Popup
import org.fxmisc.flowless.{Cell, VirtualFlow}
import org.fxmisc.richtext.event.MouseOverTextEvent
import org.fxmisc.richtext.model.TwoDimensional.Bias
import org.fxmisc.richtext.model._
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
  private val highlightedCharClass = CharCssClass("highlighted_char")

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

  private def toStylesSpans(highlight: Map[LineIdx, LineEdits], lines: IndexedSeq[String]): Option[StyleSpans[util.Collection[String]]] = {
    if (highlight.isEmpty) None
    else {
      val builder = new StyleSpansBuilder[util.Collection[String]]

      highlight.toSeq.sortBy(_._1).foreach { case (lineIdx, edits) =>
        @SuppressWarnings(Array(Warts.Var))
        var nextIdxInLine = 0
        val lineClass = charClassForLineEdit(edits.line)
        edits.charEdits.sortBy(_.from).foreach { edit =>

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
      Some(builder.create())
    }
  }
  type Par = Paragraph[util.Collection[String], String, util.Collection[String]]
  type StyledDoc = StyledDocument[util.Collection[String], String, util.Collection[String]]

  private sealed trait Which
  private case object First extends Which
  private case object Last extends Which
}
final class MyStyledDocument(paragraphs: Vector[Par])
   extends StyledDoc {
  def length(): Int = paragraphs.map(_.length()).sum

  def getText: String = paragraphs.mkString

  def getParagraphs: util.List[Par] = paragraphs.asJava

  def concat(that: StyledDoc): StyledDoc = {
    new MyStyledDocument(paragraphs ++ that.getParagraphs.asScala)
  }

  def subSequence(start: Int, end: Int): StyledDoc = {
    ???
  }

  def position(major: Int, minor: Int): TwoDimensional.Position = ???

  def offsetToPosition(offset: Int, bias: Bias): TwoDimensional.Position = ???
}

final case class LineEdits(line: LineEditType, charEdits: Vector[CharEdit])

final class Editor(maybeOtherEditor: Option[Editor]) extends CodeArea {
  this.setUseInitialStyleForInsertion(true)

  private val lineNumberFactory = new CachedLineNumberFactory(this)
  setParagraphGraphicFactory(lineNumberFactory)

  @SuppressWarnings(Array(Warts.Var))
  private var editTypes = Map.empty[LineIdx, LineEdits]

  val otherEditor: Editor = maybeOtherEditor.getOrElse(new Editor(Some(this)))

  @SuppressWarnings(Array(Warts.Var))
  private var highlightedLines: Traversable[LineIdx] = Traversable.empty

  @SuppressWarnings(Array(Warts.Var))
  private var highlightedChars: Traversable[Selection] = Traversable.empty

  @SuppressWarnings(Array(Warts.Var))
  private var _selectedForMatch: Option[Selection] = None

  @SuppressWarnings(Array(Warts.Var))
  var wordAlignmentByLine: Map[LineIdx, Traversable[MatchInfo]] = Map.empty

  @SuppressWarnings(Array(Warts.Var))
  var newLineType: String = System.lineSeparator()

  def selectedForMatch(): Option[Selection] = _selectedForMatch

  private def reset(): Unit = {
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
          otherEditor.highlightChar(fromTo)
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

  new MouseOverLineDetector(this, onEnter = { line =>
    //otherEditor.foreach(_.highlightLine(from))
    highlightLine(line)
  }, { () =>
    resetHighlighting()
  })

  addEventHandler(MouseOverTextEvent.MOUSE_OVER_TEXT_END, (e: MouseOverTextEvent) => {
    popup.hide()
    popupDebug.hide()
    otherEditor.foreach(_.resetHighlighting())
  })

  private def applyHighlight(highlight: Map[LineIdx, Traversable[CharEdit]]): Unit = {
    highlight.foreach { case (line, edits) =>
      edits.foreach { edit =>
        setCharEdit(line, edit.from, edit.to, edit.editType)
      }
    }
  }

  def setText(fullText: FullText, wordAlignment: Map[LineIdx, scala.Traversable[MatchInfo]], changed: IndexedSeq[LineIdx],
              changeType: LineEditType, moved: Set[BiasedLineMatch], notMoved: Set[LineIdx],
              highlight: Map[LineIdx, scala.Traversable[CharEdit]]): Unit = {
    reset()
    wordAlignmentByLine = wordAlignment

    changed.foreach(l => setLineType(l, changeType))
    moved.foreach(m => setLineType(m.thisSide, LineMoved(m.otherSide)))
    notMoved.foreach(l => setLineType(l, LineSame))

    applyHighlight(highlight)
    applyLineEdits(fullText)
    applyCharEdits()
    moveTo(0)
    requestFollowCaret()
    newLineType = LineEndingUtils.guessLineEnding(fullText)
  }

  private def applyLineTypeCssOnLineNumber(lineIdx: LineIdx, editType: Option[LineEdits]): Unit = {
    val lineCssClass = getLineCssClass(editType.map(_.line)).s
    val lineNoStyle = lineNumberFactory.apply(lineIdx.i).getStyleClass
    lineNoStyle.clear()
    discard(lineNoStyle.add(lineCssClass))
  }

  private def applyLineEdits(s: FullText): Unit = {
    val lines = s.s.lines.toVector
    if (lines.isEmpty) replaceText("")
    else {
      val paragraphs = lines.zip(editTypes.toSeq.sortBy(_._1)).map { case (line, (_, lineEdits)) =>
        val lineCssClass = getLineCssClass(Some(lineEdits.line)).s
        new Paragraph[util.Collection[String], String, util.Collection[String]](List(lineCssClass).asJava,
          SegmentOps.styledTextOps[util.Collection[String]](), line, List.empty[String].asJava)
      }
      replace(new MyStyledDocument(paragraphs)) //this is much faster than doing it incrementally
    }
  }

  private def setLineType(lineIdx: LineIdx, editType: LineEditType): Unit = {
    val current = editTypes.get(lineIdx)
    val newEdit = current match {
      case Some(edit) => edit.copy(line = editType)
      case None => LineEdits(editType, Vector.empty)
    }

    editTypes += lineIdx -> newEdit
    applyLineTypeCssOnLineNumber(lineIdx, Some(newEdit))
  }

  private def applyCharEdits(): Unit = {
    toStylesSpans(editTypes, this.getText().linesWithSeparators.toIndexedSeq).foreach{ spans =>
      setStyleSpans(0, spans)
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

  private def highlightLine(line: LineIdx): Unit = {
    highlightedLines ++= Traversable(line)
    setParagraphStyle(line.i, List("highlighted").asJava)
    setStyle(line.i, 0, getParagraph(line.i).length(), List(highlightedCharClass.s).asJava)

    val maybeLineEdits = editTypes.get(line)
    maybeLineEdits.foreach { lineEdits =>
      lineEdits.charEdits.foreach { charEdit =>
        charEdit.editType match {
          case _: CharsMoved | CharsSame =>
          case _ => setStyle (line.i, charEdit.from.i, charEdit.to.i, List (getCharCssClass(charEdit.editType, lineEdits.line).s).asJava)
        }
      }
    }
  }

  private def highlightChar(selection: Selection): Unit = {
    //todo this doesn't keep the color in the highlight (add/remove)
    highlightedChars ++= Traversable(selection)
    setStyle(selection.lineIdx.i, selection.from.i, selection.toExcl.i, List(highlightedCharClass.s).asJava)
  }

  private def resetHighlightingForLine(line: LineIdx): Unit = {
    val maybeLineEdits = editTypes.get(line)
    applyLineTypeCssOnLineNumber(line, maybeLineEdits)
    maybeLineEdits.foreach { lineEdits =>
      setParagraphStyle(line.i, List(getLineCssClass(Some(lineEdits.line)).s).asJava)
      setStyle(line.i, 0, getParagraph(line.i).length(), List(charClassForLineEdit(lineEdits.line).s).asJava)
      lineEdits.charEdits.foreach { charEdit =>
        setStyle(line.i, charEdit.from.i, charEdit.to.i, List(getCharCssClass(charEdit.editType, lineEdits.line).s).asJava)

        charEdit.editType match {
          case CharsMoved(_, edits) =>
            edits.foreach { moveEdit =>
              setStyle(line.i, moveEdit.from.i, moveEdit.to.i, List(getCharCssClass(moveEdit.editType, lineEdits.line).s).asJava)
            }
          case CharsInserted | CharsDeleted | CharsSame =>
        }
      }
    }
  }
  def resetHighlighting(): Unit = {
    highlightedLines.foreach { line =>
      resetHighlightingForLine(line)
    }
    highlightedLines = Traversable.empty

    //todo this doesn't work anymore as applyLineTypeCss doesn't change the char css anymore
    highlightedChars.foreach { selection =>
      resetHighlightingForLine(selection.lineIdx)
    }
    highlightedChars = Traversable.empty
  }


  def grabSelectionForMatch(): Unit = {
    val indexRange = getSelection
    val newSelection = Selection.fromAbsolute(indexRange.getStart, indexRange.getEnd, FullText(getText)).getAssert

    if (newSelection.empty) _selectedForMatch = None
    else _selectedForMatch = Some(newSelection)
  }

  // my vague understanding of why the builtin version doesn't work:
  // an edit/scroll causes the visible and the non-visible list get out of sync
  // and then the first element is not the right one.
  @SuppressWarnings(Array(Warts.Equals))
  private def robustVisibleParToAllParIndex(which: Which): Option[Int] = {
    def find[A](xs: Seq[A])(cond: A => Boolean) = {
      which match {
        case First => xs.find(cond)
        case Last => xs.reverse.find(cond)
      }
    }

    // we need to use eq here as the Java impl. does the same
    find(getVisibleParagraphs().asScala)(visibleParagraph => getParagraphs().asScala.exists(_.eq(visibleParagraph))).flatMap { firstVisibleAndValid =>
      val maybeIdx = find(getParagraphs.asScala.zipWithIndex){case (p, _) => p.eq(firstVisibleAndValid)}.map(_._2)
      maybeIdx
    }
  }

  def lineIndicesOnScreen(): LineRange = {
    // this 2 lines trigger recalculation so from the next call the results are constant in this tick
    // (bug in the framework)
    //they are equal but has a difference memory address
    List(First, Last) foreach { which =>
      robustVisibleParToAllParIndex(which).foreach(idx => allParToVisibleParIndex(idx).asScala.foreach(getVisibleParagraphBoundsOnScreen(_)))
    }
    (robustVisibleParToAllParIndex(First), robustVisibleParToAllParIndex(Last)) match {
      case (Some(first), Some(last)) => LineRange(LineIdx(first), LineIdx(last) + 1)
      case _ => LineRange.empty
    }
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

  def getTextWithGuessedLineEnding(): String = {
    getText().replace("\n", newLineType)
  }

}