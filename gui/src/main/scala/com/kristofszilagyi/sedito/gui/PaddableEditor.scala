package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common._
import com.kristofszilagyi.sedito.common.Warts.DefaultArguments
import org.fxmisc.richtext.{CodeArea, LineNumberFactory}
import scalafx.scene.Node
import PaddableEditor._
import org.fxmisc.richtext.model.TwoDimensional.Bias

import scala.language.implicitConversions
import scala.collection.JavaConverters._
import org.log4s.getLogger
object SCodeArea {
  implicit def toDelegate(sCodeArea: SCodeArea): CodeArea = sCodeArea.delegate
}


@SuppressWarnings(Array(DefaultArguments))
class SCodeArea(override val delegate: CodeArea = new CodeArea) extends Node(delegate)


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

  delegate.setParagraphGraphicFactory(LineNumberFactory.get(delegate))

  import org.fxmisc.richtext.event.MouseOverTextEvent
  import java.time.Duration

  @SuppressWarnings(Array(Warts.Var))
  private var lineInfo = Map.empty[LineIdx, LineInfo]

  delegate.setMouseOverTextDelay(Duration.ofMillis(1))
  delegate.addEventHandler(MouseOverTextEvent.MOUSE_OVER_TEXT_BEGIN, (e: MouseOverTextEvent) => {
    log.info("in")
    val chIdx = e.getCharacterIndex
    val _ = delegate.offsetToPosition(chIdx, Bias.Forward)
  })

  delegate.addEventHandler(MouseOverTextEvent.MOUSE_OVER_TEXT_END, (e: MouseOverTextEvent) => {

    log.info(s"out: $e")
  })

  def setPadding(line: Int, paddingSizeInNumberOfLines: Int): Unit = {
    val height = this.delegate.getParagraphBoxHeight(line)
    this.setParagraphBoxStyle(line, s"-fx-padding: ${height * paddingSizeInNumberOfLines} 0 0 0;")
  }

  private def applyLineTypeCss(lineIdx: LineIdx, editType: Option[EditType]): Unit = {
    delegate.setParagraphStyle(lineIdx.i, List(getCssClass(editType).s).asJava)
  }

  def setLineType(lineIdx: LineIdx, editType: EditType): Unit = {
    lineInfo += lineIdx -> (lineInfo.get(lineIdx) match {
      case Some(info) => info.copy(editType = editType)
      case None => LineInfo(Padding(0), editType)
    })
    applyLineTypeCss(lineIdx, Some(editType))
  }
}
