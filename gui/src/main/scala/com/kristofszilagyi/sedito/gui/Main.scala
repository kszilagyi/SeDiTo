package com.kristofszilagyi.sedito.gui


import java.io.File

import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common._
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
import spray.json.enrichString
import scala.collection.JavaConverters._
import scala.io.Source


object Main extends JFXApp {
  private val logger = getLogger
  logger.info("SeDiTo GUI started")
  val codeAreaLeft = PaddableEditor.test()
  codeAreaLeft.setLinePadding(1, 1)
  val codeAreaRight = PaddableEditor.test()
  val hbox = new HBox {
    spacing = 10
  }
  discard(hbox.getChildren.addAll(Seq(codeAreaLeft, codeAreaRight).asJava))

  HBox.setHgrow(codeAreaLeft, Priority.Always)
  HBox.setHgrow(codeAreaRight, Priority.Always)

  val openTestCase = new MenuItem("Open test case") {
    onAction = { _ =>
      val chooser = new DirectoryChooser()
      chooser.setTitle("Choose directory")
      val directory = chooser.showDialog(stage)
      //TODO error handling
      val left = Source.fromFile(new File(directory, "left.txt")).mkString
      val right = Source.fromFile(new File(directory, "right.txt")).mkString
      val alignment = Source.fromFile(new File(directory, "alignment.json")).mkString.parseJson.convertTo[Alignment]
      codeAreaLeft.replaceText(left)
      codeAreaRight.replaceText(right)
      val deleted = (0 until codeAreaLeft.getParagraphs.size()).map(LineIdx.apply).filterNot(l => alignment.matches.map(_.leftLineIdx).contains(l))
      val inserted = (0 until codeAreaRight.getParagraphs.size()).map(LineIdx.apply).filterNot(l => alignment.matches.map(_.rightLineIdx).contains(l))
      val partitioned = alignment.partition
      val moved = partitioned.moved
      val notMovedLeft = partitioned.notMoved.map(_.leftLineIdx)
      val notMovedRight = partitioned.notMoved.map(_.rightLineIdx)
      deleted.foreach(l => codeAreaLeft.setLineType(l, Deleted))
      inserted.foreach(l => codeAreaRight.setLineType(l, Inserted))
      moved.foreach(m => codeAreaLeft.setLineType(m.leftLineIdx, Moved(m.rightLineIdx)))
      moved.foreach(m => codeAreaRight.setLineType(m.rightLineIdx, Moved(m.leftLineIdx)))
      notMovedLeft.foreach(l => codeAreaLeft.setLineType(l, Same))
      notMovedRight.foreach(l => codeAreaRight.setLineType(l, Same))
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
