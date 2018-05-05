package com.kristofszilagyi.sedito.gui

import java.io.File

import com.kristofszilagyi.sedito.common.TestCase
import com.kristofszilagyi.sedito.common.Warts.discard
import javafx.scene.control.Alert
import javafx.stage.DirectoryChooser
import org.log4s.getLogger
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Menu, MenuBar, MenuItem}
import scalafx.scene.layout.BorderPane

import scala.util.{Failure, Success}

final class MainWindow {
  private val logger = getLogger

  private val diffPane = new DiffPane

  private val openTestCase = new MenuItem("Open test case") {
    onAction = { _ =>
      val chooser = new DirectoryChooser()
      chooser.setTitle("Choose directory")
      val testDir = new File("common/src/test/resources/algorithm_tests/full_tests")
      if (testDir.isDirectory) {
        chooser.setInitialDirectory(new File(testDir.getPath))
      }
      val directory: File = chooser.showDialog(stage)
      TestCase.open(directory.toPath) match {
        case Success(testCase) =>
          diffPane.openTestCase(testCase.left, testCase.right, testCase.wordAlignment.toUnambigous)
        case Failure(e) =>
          logger.error(e)("Failed to open test case")
          discard(new Alert(AlertType.Error, s"Failed to open test: $e").showAndWait())
      }
    }
  }
  private val fileMenu = new Menu("File") {
    items = List(openTestCase)
  }


  private val url = getClass.getClassLoader.getResource("simple.css").toExternalForm
  private val stage: PrimaryStage = new PrimaryStage {
    maximized = true
    scene = new Scene {

      discard(stylesheets.add(url))
      root = new BorderPane {
        top = new MenuBar {
          useSystemMenuBar = true
          menus = List(fileMenu)
        }
        center = diffPane
      }
    }
  }
  stage.show()
}
