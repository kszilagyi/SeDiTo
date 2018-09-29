package com.kristofszilagyi.sedito.gui


import com.kristofszilagyi.sedito.common.Warts
import com.sun.javafx.css.CssError
import javafx.application.Application
import javafx.collections.ListChangeListener
import org.log4s._
import scala.collection.JavaConverters._
@SuppressWarnings(Array(Warts.Null))
object Main extends Application {

  def main(args: Array[String]): Unit = {
    Application.launch(args:_*)
  }
  private val logger = getLogger

  def start(stage: _root_.javafx.stage.Stage): Unit = {
    logger.info("SeDiTo GUI started")
    new MainWindow()


    com.sun.javafx.css.StyleManager.errorsProperty().addListener(new ListChangeListener[CssError] {
      def onChanged(change: ListChangeListener.Change[_ <: CssError]): Unit = {
        change.getList.asScala.foreach { error =>
          logger.info(s"Css error: $error")
        }
      }
    })
  }
}
