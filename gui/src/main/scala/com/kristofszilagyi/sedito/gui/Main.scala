package com.kristofszilagyi.sedito.gui


import javafx.collections.ListChangeListener
import com.kristofszilagyi.sedito.common.Warts._
import com.sun.javafx.css.CssError
import javafx.stage.DirectoryChooser
import org.fxmisc.richtext.{CodeArea, LineNumberFactory}
import org.log4s._

import scala.language.implicitConversions
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.control.{Menu, MenuBar, MenuItem}
import scalafx.scene.layout.{BorderPane, HBox, Priority}
import scalafx.scene.{Node, Scene}

object SCodeArea {
  implicit def toDelegate(sCodeArea: SCodeArea): CodeArea = sCodeArea.delegate
  def createCustom(): SCodeArea = { //todo extract to different type?
    val codeArea = new SCodeArea()
    codeArea.setParagraphGraphicFactory(LineNumberFactory.get(codeArea))
    codeArea.appendText("that's a lot\n of text\n so many\ntext")
    //codeArea.setParagraphStyle(1, List("red").asJava)
    codeArea.setParagraphBoxStyle(1, "-fx-padding: 20 0 0 0;")
    codeArea
  }
}

@SuppressWarnings(Array(DefaultArguments))
final class SCodeArea(override val delegate: CodeArea = new CodeArea) extends Node(delegate)

object Main extends JFXApp {
  private val logger = getLogger
  logger.info("SeDiTo GUI started")
  val codeArea1 = SCodeArea.createCustom()
  val codeArea2 = SCodeArea.createCustom()
  val hbox = new HBox {
    children = Seq(codeArea1, codeArea2)
    spacing = 10
  }
  HBox.setHgrow(codeArea1, Priority.Always)
  HBox.setHgrow(codeArea2, Priority.Always)

  val openTestCase = new MenuItem("Open test case") {
    onAction = { _ =>
      val chooser = new DirectoryChooser()
      chooser.setTitle("Choose directory")
      val _ = chooser.showDialog(stage)
    }
  }

  val fileMenu = new Menu("File") {
    items = List(openTestCase)
  }


  val url = getClass.getClassLoader.getResource("simple.css").toExternalForm
  stage = new PrimaryStage {
    maximized = true
    scene = new Scene {
      discard(stylesheets += url )
      root = new BorderPane {
        top = new MenuBar {
          useSystemMenuBar = true
          menus = List(fileMenu)
        }
        center = hbox
      }
    }
  }


  com.sun.javafx.css.StyleManager.errorsProperty().addListener(new ListChangeListener[CssError] {
    def onChanged(change: ListChangeListener.Change[_ <: CssError]): Unit = {
      change.getList.foreach{ error =>
        logger.info(s"Css error: $error")
      }
    }
  })
}
