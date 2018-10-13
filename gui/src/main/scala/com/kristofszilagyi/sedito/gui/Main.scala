package com.kristofszilagyi.sedito.gui


import java.io.ObjectInputStream
import com.kristofszilagyi.sedito.aligner.HardcodedScaler
import com.kristofszilagyi.sedito.aligner.Aligner
import com.kristofszilagyi.sedito.common.{FullText, Warts}
import com.sun.javafx.css.CssError
import javafx.application.{Application, Platform}
import javafx.collections.ListChangeListener
import org.log4s._
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

import Main._
import com.kristofszilagyi.sedito.common.utils.Control.using
import smile.classification.NeuralNetwork
import smile.feature.Scaler

import scala.collection.JavaConverters._
@SuppressWarnings(Array(Warts.Null))
final class Main extends Application {
  private val logger = getLogger

  def start(stage: _root_.javafx.stage.Stage): Unit = {
    logger.info("SeDiTo GUI started")
    Thread.setDefaultUncaughtExceptionHandler((t: Thread, e: Throwable) => {
      logger.error(e)("Exception in thread \"" + t.getName + "\"")
    })

    val mainWindow = new MainWindow()
    val args = getParameters.getRaw
    logger.info(s"Args: $args")
    if (args.size() >= 2) {
      Platform.runLater { () =>
        val left = FullText(new String(Files.readAllBytes(Paths.get(args.get(0))), StandardCharsets.UTF_8))
        val right = FullText(new String(Files.readAllBytes(Paths.get(args.get(1))), StandardCharsets.UTF_8))
        val (classifier, scaler) = loadAI()
        val calculatedAlignment = new Aligner(classifier, scaler).align(left, right)
        mainWindow.setContent(left, right, calculatedAlignment)
      }
    }

    com.sun.javafx.css.StyleManager.errorsProperty().addListener(new ListChangeListener[CssError] {
      def onChanged(change: ListChangeListener.Change[_ <: CssError]): Unit = {
        change.getList.asScala.foreach { error =>
          logger.info(s"Css error: $error")
        }
      }
    })
  }
}

object Main {

  @SuppressWarnings(Array(Warts.AsInstanceOf))
  def loadAI(): (NeuralNetwork, Scaler) = {
    val classifier = using(new ObjectInputStream(getClass.getClassLoader.getResourceAsStream("model.model"))) { stream =>
      stream.readObject().asInstanceOf[NeuralNetwork]
    }
    val scaler = new HardcodedScaler();
    (classifier, scaler)
  }

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Main], args:_*)
  }
}
