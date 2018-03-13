package com.kristofszilagyi.sedito.gui

import java.util
import java.util.Collections

import com.kristofszilagyi.sedito.common.Warts.{DefaultArguments, discard}
import com.kristofszilagyi.sedito.common._
import com.kristofszilagyi.sedito.gui.PaddableEditor._
import javafx.scene.control.Label
import javafx.scene.input.ScrollEvent
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

  private def getLineCssClass(editType: Option[EditType]) = {
    (editType map {
      case _: Moved => LineCssClass("moved")
      case Inserted => LineCssClass("inserted")
      case Deleted => LineCssClass("deleted")
      case Same => LineCssClass("same")
    }).getOrElse(LineCssClass("white"))
  }

  private def getCharCssClass(editType: Option[EditType]) = {
    getLineCssClass(editType).toChar
  }
}


final class PaddableEditor extends SCodeArea {

  setParagraphGraphicFactory(LineNumberFactory.get(this))

  import java.time.Duration

  import org.fxmisc.richtext.event.MouseOverTextEvent

  @SuppressWarnings(Array(Warts.Var))
  private var editTypes = Map.empty[LineIdx, EditType]

  @SuppressWarnings(Array(Warts.Var))
  private var paddings = Map.empty[LineIdx, NumberOfLinesPadding]

  @SuppressWarnings(Array(Warts.Var))
  private var otherEditor: Option[PaddableEditor] = None

  @SuppressWarnings(Array(Warts.Var))
  private var highlightedLines: Traversable[LineIdx] = Traversable.empty

  def reset(): Unit = {
    editTypes = Map.empty
    paddings = Map.empty
    highlightedLines = Traversable.empty
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
        edit match {
          case Moved(from) =>
            popupMsg.setText(s"Moved from line ${from.i + 1}")
            popup.show(this, posOnScreen.getX, posOnScreen.getY + 10)
            otherEditor.foreach(_.highlightLine(from))
          case Inserted | Deleted | Same=>
        }
      case None =>
    }
  })

  discard(plainTextChanges.subscribe(_ => applyAllPadding()))

  //disable scroll
  this.addEventFilter(ScrollEvent.ANY, (e: ScrollEvent) => {
    e.consume()
  })


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

  private def applyLineTypeCss(lineIdx: LineIdx, editType: Option[EditType]): Unit = {
    setParagraphStyle(lineIdx.i, List(getLineCssClass(editType).s).asJava)
    paddings.get(lineIdx).foreach(applyPaddingCss(lineIdx, _))
  }

  def setLineType(lineIdx: LineIdx, editType: EditType): Unit = {
    editTypes += lineIdx -> editType
    applyLineTypeCss(lineIdx, Some(editType))
  }

  def setCharCss(lineIdx: LineIdx, from: CharIdxInLine, to: CharIdxInLine, editType: EditType): Unit = {
    setStyle(lineIdx.i, from.i, to.i, List(getCharCssClass(Some(editType)).s).asJava)
  }

  def highlightLine(lineIdx: LineIdx): Unit = {
    highlightedLines ++= Traversable(lineIdx)
    setParagraphStyle(lineIdx.i, List("highlighted").asJava)
  }

  def resetHighlighting(): Unit = {
    highlightedLines.foreach { line =>
      applyLineTypeCss(line, editTypes.get(line))
    }
    highlightedLines = Traversable.empty
  }

  def setOther(other: PaddableEditor): Unit = {
    otherEditor = Some(other)
  }

}
