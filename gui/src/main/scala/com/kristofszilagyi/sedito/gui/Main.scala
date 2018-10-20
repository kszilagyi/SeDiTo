package com.kristofszilagyi.sedito.gui


import java.io.{FileNotFoundException, RandomAccessFile}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import java.util.Base64
import java.util.concurrent.ConcurrentLinkedQueue

import com.kristofszilagyi.sedito.aligner.{Aligner, HardcodedNeuralNetwork, HardcodedScaler}
import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.common.utils.Control._
import com.kristofszilagyi.sedito.common.{FullText, Warts}
import com.kristofszilagyi.sedito.gui.JavaFxOps.schedule
import com.kristofszilagyi.sedito.gui.Main._
import com.sun.javafx.css.CssError
import javafx.application.Application
import javafx.collections.ListChangeListener
import javafx.scene.control.Alert
import javafx.scene.control.Alert.AlertType
import org.log4s._
import smile.classification.SoftClassifier
import smile.feature.Scaler

import scala.collection.JavaConverters._
import scala.concurrent.duration.DurationInt
@SuppressWarnings(Array(Warts.Null))
final class Main extends Application {

  def start(stage: _root_.javafx.stage.Stage): Unit = {
    logger.info("SeDiTo GUI started")
    Thread.setDefaultUncaughtExceptionHandler((t: Thread, e: Throwable) => {
      logger.error(e)("Exception in thread \"" + t.getName + "\"")
    })

    val mainWindow = new MainWindow()
    val args = getParameters.getRaw
    val (classifier, scaler) = loadAI()

    openFromArgs(args.asScala.toList, mainWindow, classifier, scaler)

    com.sun.javafx.css.StyleManager.errorsProperty().addListener(new ListChangeListener[CssError] {
      def onChanged(change: ListChangeListener.Change[_ <: CssError]): Unit = {
        change.getList.asScala.foreach { error =>
          logger.info(s"Css error: $error")
        }
      }
    })
    val openerQueue = new ConcurrentLinkedQueue[String]
    startNamedPipeReading(openerQueue)

    schedule(10.millis, () => {
      val maybeArgLine = Option(openerQueue.poll())
      maybeArgLine.foreach{ argline =>
        val splitArgs = argline.split(" ").toList.map { arg =>
          val decodeByteArray = Base64.getDecoder.decode(arg.getBytes(UTF_8))
          new String(decodeByteArray, UTF_8)
        }
        openFromArgs(splitArgs, mainWindow, classifier, scaler)
      }
    })
    logger.info(s"Main.start finished")
  }
}

object Main {
  private val logger = getLogger

  @SuppressWarnings(Array(Warts.AsInstanceOf))
  def loadAI(): (SoftClassifier[Array[Double]], Scaler) = {
    val classifier = HardcodedNeuralNetwork.nn
    val scaler = new HardcodedScaler()
    (classifier, scaler)
  }

  @SuppressWarnings(Array(Warts.While))
  private def startNamedPipeReading(openerQueue: ConcurrentLinkedQueue[String]): Unit = {
    val thread = new Thread(
      () => {
        while (true) {
          //todo make this work on windows (and MAC?)
          val filePath = s"${System.getProperty("user.home")}/.sedito/startup_pipe"
          try {
            using(new RandomAccessFile(filePath, "r")) { pipe =>
              //todo this doesn't work for non-ASCII filenames
              val line = pipe.readLine()
              logger.info(s"Received parameters: $line")
              discard(openerQueue.add(line))
            }
          } catch {
            case _: FileNotFoundException => logger.info(s"$filePath not found")
            case t: Throwable => logger.warn(t)("Pipe failure:")
          }
          Thread.sleep(10)
        }
      }
    )
    thread.start()
  }
  private def openFromArgs(args: List[String], mainWindow: MainWindow, classifier: SoftClassifier[Array[Double]], scaler: Scaler): Unit = {
    logger.info(s"Args: $args")
    args match {
      case leftFilename :: rightFilename :: Nil =>
        val left = FullText(new String(Files.readAllBytes(Paths.get(leftFilename)), UTF_8))
        val right = FullText(new String(Files.readAllBytes(Paths.get(rightFilename)), UTF_8))
        // todo this should not be on the main thread
        val calculatedAlignment = new Aligner(classifier, scaler).align(left, right)
        mainWindow.setContent(left, right, calculatedAlignment)
      case Nil =>
      case other =>
        val msg = s"Unsupported argument format: $other"
        logger.warn(msg)
        val alert = new Alert(AlertType.ERROR, msg)
        alert.show()
    }

  }

  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Main], args:_*)
  }
}
