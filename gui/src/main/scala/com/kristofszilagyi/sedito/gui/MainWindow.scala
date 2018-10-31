package com.kristofszilagyi.sedito.gui

import java.io.File

import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.common.{FullText, TestCase, UnambiguousWordAlignment}
import com.kristofszilagyi.sedito.gui.JavaFxOps.menuItem
import javafx.scene.Scene
import javafx.scene.control.Alert.AlertType
import javafx.scene.control.{Alert, Menu, MenuBar}
import javafx.scene.layout.BorderPane
import javafx.stage.{DirectoryChooser, Stage}
import org.log4s.getLogger

import scala.collection.JavaConverters._
import scala.util.{Failure, Success}

final class MainWindow {
  private val logger = getLogger

  private val diffPane = new DiffPane
  private val testDir = new File("common/src/test/resources/algorithm_tests/full_tests")

  private val openTestCase = menuItem("Open test case",
    { _ =>
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
          setContent(testCase.left, testCase.right, unambiguousWordAlignment)
        case Failure(e) =>
          logger.error(e)("Failed to open test case")
          discard(new Alert(AlertType.ERROR, s"Failed to open test: $e").showAndWait())
      }
    }
  )

  private val saveTestCase = menuItem("Save test case",
    { _ =>
      val chooser = new DirectoryChooser()
      chooser.setTitle("Choose directory")
      if (testDir.isDirectory) {
        chooser.setInitialDirectory(new File(testDir.getPath))
      }
      val directory: File = chooser.showDialog(stage)
      discard(diffPane.testCase.save(directory.toPath))
    }
  )

  private val open =  menuItem("Open",
    { _ =>
      ()
    }
  )
  private val fileMenu = {
    val m = new Menu("File")
    discard(m.getItems.addAll(List(open).asJava))
    m
  }
  private val devMenu = {
    val m = new Menu("Dev")
    discard(m.getItems.addAll(List(openTestCase, saveTestCase).asJava))
    m
  }

  private val url = getClass.getClassLoader.getResource("simple.css").toExternalForm
  private val stage: Stage = new Stage
  stage.setMaximized(true)
  stage.setScene{
    val menuBar = new MenuBar(fileMenu, devMenu)

    menuBar.setUseSystemMenuBar(true)

    val borderPane = new BorderPane

    borderPane.setTop(menuBar)
    borderPane.setCenter(diffPane)

    val scene = new Scene(borderPane)

    //noinspection JavaAccessorMethodCalledAsEmptyParen
    discard(scene.getStylesheets().add(url))
    scene
  }

  def setTitle(title: String): Unit = stage.setTitle(title)

  def setContent(left: FullText, right: FullText, alignment: UnambiguousWordAlignment): Unit = {
    val showing = stage.isShowing
    diffPane.openTestCase(left, right, alignment, showing = showing)
    if (!showing) stage.show()
  }

  def isVisible: Boolean = stage.isShowing
}
