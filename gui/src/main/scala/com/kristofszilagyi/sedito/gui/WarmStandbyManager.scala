package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.Warts
import com.kristofszilagyi.sedito.gui.JavaFxOps.scheduleOnJavaFxThread
import org.log4s.getLogger

import scala.concurrent.duration.DurationInt
import WarmStandbyManager._
import javafx.application.Platform

object WarmStandbyManager {
  private val logger = getLogger

}
// the creation of this call and every method call on it has to run on the JavaFx thread
final class WarmStandbyManager(mainWindow: MainWindow) {
  private val period = 5.seconds
  private val standbyTime = 10.minutes

  @SuppressWarnings(Array(Warts.Var))
  private var durationUntilQuit = standbyTime

  scheduleOnJavaFxThread(period, () => {
    if(mainWindow.isVisible) {
      logger.info(s"Window visible, extending")
      extend()
    }
    else {
      durationUntilQuit -= period
      logger.info(s"Window not visible, time remaining: $durationUntilQuit")
      if(durationUntilQuit < 0.second) {
        //todo this is not 100% correct: if an opening is just happening it will be lost. The impact is not too hight though.
        logger.info(s"Not used for $standbyTime, exiting.")
        Platform.exit()
      }
    }
  })

  def extend(): Unit = durationUntilQuit = standbyTime
}
