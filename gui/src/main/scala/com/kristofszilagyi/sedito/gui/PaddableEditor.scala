package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.Warts.DefaultArguments
import org.fxmisc.richtext.{CodeArea, LineNumberFactory}
import scalafx.scene.Node
import scala.language.implicitConversions


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
}

class PaddableEditor extends SCodeArea {
  delegate.setParagraphGraphicFactory(LineNumberFactory.get(delegate))
  def setPadding(line: Int, paddingSizeInNumberOfLines: Int): Unit = {
    val height = this.delegate.getParagraphBoxHeight(line)
    this.setParagraphBoxStyle(line, s"-fx-padding: ${height * paddingSizeInNumberOfLines} 0 0 0;")
  }

}
