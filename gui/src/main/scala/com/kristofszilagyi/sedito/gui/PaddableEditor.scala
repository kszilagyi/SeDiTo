package com.kristofszilagyi.sedito.gui

import java.util
import java.util.Collections

import com.kristofszilagyi.sedito.common.Warts.{DefaultArguments, discard}
import com.kristofszilagyi.sedito.common._
import com.kristofszilagyi.sedito.gui.PaddableEditor._
import javafx.scene.control.Label
import javafx.scene.text.TextFlow
import javafx.stage.Popup
import org.fxmisc.richtext.model.TwoDimensional.Bias
import org.fxmisc.richtext.model.{SegmentOps, SimpleEditableStyledDocument}
import org.fxmisc.richtext.{GenericStyledArea, LineNumberFactory, StyledTextArea}
import org.log4s.getLogger

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

  final case class CssClass(s: String)
  private def getCssClass(editType: Option[EditType]) = {
    (editType map {
      case _: Moved => CssClass("moved")
      case Inserted => CssClass("inserted")
      case Deleted => CssClass("deleted")
      case Same => CssClass("same")
    }).getOrElse(CssClass("white"))
  }
}

final case class Padding(i: Int)
final case class LineInfo(padding: Padding, editType: EditType)

final class PaddableEditor extends SCodeArea {
  private val log = getLogger

  setParagraphGraphicFactory(LineNumberFactory.get(this))
  //setParagraphGraphicFactory(PaddableLineNumberFactory.get(this))

  import java.time.Duration

  import org.fxmisc.richtext.event.MouseOverTextEvent

  @SuppressWarnings(Array(Warts.Var))
  private var lineInfo = Map.empty[LineIdx, LineInfo]

  private val popup = new Popup
  private val popupMsg = new Label
  popupMsg.setStyle("-fx-background-color: black;" + "-fx-text-fill: white;" + "-fx-padding: 5;")
  discard(popup.getContent.add(popupMsg))
  setMouseOverTextDelay(Duration.ofMillis(1))
  addEventHandler(MouseOverTextEvent.MOUSE_OVER_TEXT_BEGIN, (e: MouseOverTextEvent) => {

    log.info("in")
    val chIdx = e.getCharacterIndex
    val posOnScreen = e.getScreenPosition
    val posInText = offsetToPosition(chIdx, Bias.Forward)
    lineInfo.get(LineIdx(posInText.getMajor)) match {
      case Some(info) =>
        info.editType match {
          case Moved(from) =>
            popupMsg.setText(s"Moved from line ${from.i + 1}")
            popup.show(this, posOnScreen.getX, posOnScreen.getY + 10)
          case Inserted | Deleted | Same=>
        }
      case None =>
    }
  })

  addEventHandler(MouseOverTextEvent.MOUSE_OVER_TEXT_END, (e: MouseOverTextEvent) => {
    log.info(s"out: $e")
    popup.hide()
  })

  def setLinePadding(line: Int, paddingSizeInNumberOfLines: Int): Unit = {
    val height = getParagraphBoxHeight(line)
    this.setParagraphBoxStyle(line, s"-fx-padding: ${height * paddingSizeInNumberOfLines} 0 0 0;")
  }

  private def applyLineTypeCss(lineIdx: LineIdx, editType: Option[EditType]): Unit = {
    setParagraphStyle(lineIdx.i, List(getCssClass(editType).s).asJava)
  }

  def setLineType(lineIdx: LineIdx, editType: EditType): Unit = {
    lineInfo += lineIdx -> (lineInfo.get(lineIdx) match {
      case Some(info) => info.copy(editType = editType)
      case None => LineInfo(Padding(0), editType)
    })
    applyLineTypeCss(lineIdx, Some(editType))
  }
}
