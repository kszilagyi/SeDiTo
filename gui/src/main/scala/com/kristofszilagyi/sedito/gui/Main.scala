package com.kristofszilagyi.sedito.gui


import com.sun.javafx.css.CssError
import javafx.collections.ListChangeListener
import org.log4s._
import scalafx.Includes._
import scalafx.application.JFXApp

object Main extends JFXApp {

  private val logger = getLogger
  logger.info("SeDiTo GUI started")
  new MainWindow()
  new MainWindow()




  com.sun.javafx.css.StyleManager.errorsProperty().addListener(new ListChangeListener[CssError] {
    def onChanged(change: ListChangeListener.Change[_ <: CssError]): Unit = {
      change.getList.foreach{ error =>
        logger.info(s"Css error: $error")
      }
    }
  })
}
