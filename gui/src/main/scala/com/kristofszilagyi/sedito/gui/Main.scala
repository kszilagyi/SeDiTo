package com.kristofszilagyi.sedito.gui


import com.kristofszilagyi.sedito.common.Warts
import org.fxmisc.richtext.CodeArea

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.layout.BorderPane
import scalafx.scene.{Node, Scene}
import org.fxmisc.richtext.LineNumberFactory

@SuppressWarnings(Array(Warts.DefaultArguments))
final class SCodeArea(override val delegate: CodeArea = new CodeArea) extends Node(delegate)

object Main extends JFXApp {
  val codeArea = new SCodeArea()

  codeArea.delegate.setParagraphGraphicFactory(LineNumberFactory.get(codeArea.delegate))
  stage = new PrimaryStage {
    scene = new Scene {
      root = new BorderPane {
        padding = Insets(25)
        center = codeArea
      }
    }
  }
}
