package com.kristofszilagyi.sedito.gui

import java.io.File
import java.nio.file.Path

import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.common.{FullText, TestCase, UnambiguousWordAlignment}
import com.kristofszilagyi.sedito.gui.JavaFxOps.menuItem
import com.kristofszilagyi.sedito.gui.utils.Buttons
import javafx.scene.Scene
import javafx.scene.control.Alert.AlertType
import javafx.scene.control._
import javafx.scene.layout.{BorderPane, VBox}
import javafx.scene.text.Font
import javafx.stage.{DirectoryChooser, Stage}
import org.log4s.getLogger

import scala.collection.JavaConverters._
import scala.util.{Failure, Success}

final class MainWindow {
  private val logger = getLogger

  private val diffPane = new DiffPane
  private val testDir = new File("common/src/test/resources/algorithm_tests/full_tests")

  assert(Font.loadFont(this.getClass.getResource("/fa-solid-900.ttf").toExternalForm, 12) != null, "Couldn't load font awesome")

  private def menuBar = {
    val openTestCase = menuItem("Open test case",
      { _ =>
        val chooser = new DirectoryChooser()
        chooser.setTitle("Choose directory")
        if (testDir.isDirectory) {
          chooser.setInitialDirectory(new File(testDir.getPath))
        }
        val directory = chooser.showDialog(stage).toPath
        TestCase.open(directory) match {
          case Success(testCase) =>
            val unambiguousWordAlignment = testCase.wordAlignment.toUnambiguous
            logger.info(s"Reducing conflict: ${testCase.wordAlignment.matches.size} to ${unambiguousWordAlignment.matches.size}")
            setContent(testCase.left, testCase.right, TestCase.leftPath(directory), TestCase.rightPath(directory), unambiguousWordAlignment)
          case Failure(e) =>
            logger.error(e)("Failed to open test case")
            discard(new Alert(AlertType.ERROR, s"Failed to open test: $e").showAndWait())
        }
      }
    )

    val saveTestCase = menuItem("Save test case",
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

    val open =  menuItem("Open",
      { _ =>
        ()
      }
    )
    val fileMenu = {
      val m = new Menu("File")
      discard(m.getItems.addAll(List(open).asJava))
      m
    }
    val devMenu = {
      val m = new Menu("Dev")
      discard(m.getItems.addAll(List(openTestCase, saveTestCase).asJava))
      m
    }
    val menuBar = new MenuBar(fileMenu, devMenu)
    menuBar.setUseSystemMenuBar(true)
    menuBar
  }

  private def toolBar = {
    val up = Buttons.awesome('\uf062')
    val down = Buttons.awesome('\uf063')

    val toolBar = new ToolBar(
      up, down
    )
    toolBar
  }


  private val url = getClass.getClassLoader.getResource("simple.css").toExternalForm
  private val stage: Stage = new Stage
  stage.setMaximized(true)
  stage.setScene{
    val menuAndTools = new VBox(menuBar, toolBar)

    val borderPane = new BorderPane
    borderPane.setTop(menuAndTools)
    borderPane.setCenter(diffPane)

    val scene = new Scene(borderPane)

    //noinspection JavaAccessorMethodCalledAsEmptyParen
    discard(scene.getStylesheets().add(url))
    scene
  }

  stage.setOnHidden { _ =>
    val (leftRes, rightRes) = diffPane.saveFiles()

    val error = List(leftRes, rightRes).zip(List("left", "right")) flatMap { case (res, side) =>
      res match {
        case Saved => None
        case SaveFailed(t) => Some(s"Save $side file failed. Error: $t")
        case NoPath => Some(s"Couldn't save $side content as it has no corresponding file.") // todo ask for place to save
      }
    }
    if (error.nonEmpty) {
      val alert = new Alert(Alert.AlertType.ERROR, error.mkString("\n"))
      discard(alert.showAndWait())
    }
  }

  def setTitle(title: String): Unit = stage.setTitle(title)

  def setContent(left: FullText, right: FullText, leftPath: Path, rightPath: Path, alignment: UnambiguousWordAlignment): Unit = {
    val showing = stage.isShowing
    diffPane.open(left, right, Some(leftPath), Some(rightPath), alignment, showing = showing)
    if (!showing) stage.show()
  }

  def isVisible: Boolean = stage.isShowing
}