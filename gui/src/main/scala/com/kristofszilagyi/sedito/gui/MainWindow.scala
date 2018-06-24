package com.kristofszilagyi.sedito.gui

import java.io.File

import com.kristofszilagyi.sedito.common.{FullText, TestCase, UnambiguousWordAlignment}
import com.kristofszilagyi.sedito.common.Warts.discard
import javafx.scene.control.Alert
import javafx.stage.DirectoryChooser
import org.log4s.getLogger
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Menu, MenuBar, MenuItem}
import scalafx.scene.layout.BorderPane
import scalafx.stage.Stage

import scala.util.{Failure, Success}

final class MainWindow {
  private val logger = getLogger

  private val diffPane = new DiffPane
  private val testDir = new File("common/src/test/resources/algorithm_tests/full_tests")

  private val openTestCase = new MenuItem("Open test case") {
    onAction = { _ =>
      val chooser = new DirectoryChooser()
      chooser.setTitle("Choose directory")
      if (testDir.isDirectory) {
        chooser.setInitialDirectory(new File(testDir.getPath))
      }
      val directory: File = chooser.showDialog(stage)
      TestCase.open(directory.toPath) match {
        case Success(testCase) =>
          val unambiguousWordAlignment = testCase.wordAlignment.toUnambigous
          logger.info(s"Reducing conflict: ${testCase.wordAlignment.matches.size} to ${unambiguousWordAlignment.matches.size}")
          diffPane.openTestCase(testCase.left, testCase.right, unambiguousWordAlignment)
        case Failure(e) =>
          logger.error(e)("Failed to open test case")
          discard(new Alert(AlertType.Error, s"Failed to open test: $e").showAndWait())
      }
    }
  }

  private val saveTestCase = new MenuItem("Save test case") {
    onAction = { _ =>
      val chooser = new DirectoryChooser()
      chooser.setTitle("Choose directory")
      if (testDir.isDirectory) {
        chooser.setInitialDirectory(new File(testDir.getPath))
      }
      val directory: File = chooser.showDialog(stage)
      discard(diffPane.testCase.save(directory.toPath))
    }
  }
  private val fileMenu = new Menu("File") {
    items = List(openTestCase, saveTestCase)
  }


  private val url = getClass.getClassLoader.getResource("simple.css").toExternalForm
  private val stage: Stage = new Stage {
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

  def setTitle(title: String): Unit = stage.setTitle(title)

  def setContent(left: FullText, right: FullText, alignment: UnambiguousWordAlignment): Unit = {
    diffPane.openTestCase(left, right, alignment)
  }
}
