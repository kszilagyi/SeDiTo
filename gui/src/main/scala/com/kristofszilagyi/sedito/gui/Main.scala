package com.kristofszilagyi.sedito.gui


import com.kristofszilagyi.sedito.common.Warts._
import com.sun.javafx.css.CssError
import javafx.collections.ListChangeListener
import javafx.stage.DirectoryChooser
import org.log4s._
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.{Menu, MenuBar, MenuItem}
import scalafx.scene.layout.{BorderPane, HBox, Priority}


object Main extends JFXApp {
  private val logger = getLogger
  logger.info("SeDiTo GUI started")
  val codeArea1 = PaddableEditor.test()
  codeArea1.setPadding(1, 1)
  val codeArea2 = PaddableEditor.test()
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
