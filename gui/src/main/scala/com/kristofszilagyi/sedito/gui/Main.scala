package com.kristofszilagyi.sedito.gui


import java.io.File

import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common._
import com.sun.javafx.css.CssError
import javafx.collections.ListChangeListener
import javafx.scene.control.Alert
import javafx.stage.DirectoryChooser
import org.log4s._
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Menu, MenuBar, MenuItem}
import scalafx.scene.layout.BorderPane

import scala.util.{Failure, Success}

object Main extends JFXApp {

  private val logger = getLogger
  logger.info("SeDiTo GUI started")

  val diffPane = new DiffPane

  val openTestCase = new MenuItem("Open test case") {
    onAction = { _ =>
      val chooser = new DirectoryChooser()
      chooser.setTitle("Choose directory")
      val testDir = new File("common/src/test/resources/algorithm_tests/full_tests")
      if (testDir.isDirectory) {
        chooser.setInitialDirectory(new File(testDir.getPath))
      }
      val directory = chooser.showDialog(stage)
      TestCase.open(directory.toPath) match {
        case Success(testCase) =>
          diffPane.openTestCase(testCase.left, testCase.right, testCase.wordAlignment)
        case Failure(e) =>
          logger.error(e)("Failed to open test case")
          discard(new Alert(AlertType.Error, s"Failed to open test: $e").showAndWait())
      }
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
        center = diffPane
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
