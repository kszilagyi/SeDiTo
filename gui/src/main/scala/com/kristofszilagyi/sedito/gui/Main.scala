package com.kristofszilagyi.sedito.gui


import com.kristofszilagyi.sedito.common.Warts
import org.fxmisc.richtext.{CodeArea, LineNumberFactory}

import scala.language.implicitConversions
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.layout.{HBox, Priority}
import scalafx.scene.{Node, Scene}

object SCodeArea {
  implicit def toDelegate(sCodeArea: SCodeArea): CodeArea = sCodeArea.delegate
  def createCustom(): SCodeArea = { //todo extract to different type?
    val codeArea = new SCodeArea()
    codeArea.setParagraphGraphicFactory(LineNumberFactory.get(codeArea))
    codeArea
  }
}

@SuppressWarnings(Array(Warts.DefaultArguments))
final class SCodeArea(override val delegate: CodeArea = new CodeArea) extends Node(delegate)

object Main extends JFXApp {

  val codeArea1 = SCodeArea.createCustom()
  val codeArea2 = SCodeArea.createCustom()
  val hbox = new HBox {
    children = Seq(codeArea1, codeArea2)
    spacing = 10
  }
  HBox.setHgrow(codeArea1, Priority.Always)
  HBox.setHgrow(codeArea2, Priority.Always)

  stage = new PrimaryStage {
    maximized = true
    scene = new Scene {
      root = hbox
    }
  }
}
