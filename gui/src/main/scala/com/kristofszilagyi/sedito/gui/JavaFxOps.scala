package com.kristofszilagyi.sedito.gui

import javafx.animation.{Animation, KeyFrame, Timeline}
import javafx.event.ActionEvent
import javafx.scene.control.MenuItem
import javafx.util.Duration

object JavaFxOps {
  def menuItem(title: String, action: ActionEvent => Unit): MenuItem = {
    val i = new MenuItem(title)
    i.setOnAction(action(_))
    i
  }

  def scheduleOnJavaFxThread(period: scala.concurrent.duration.Duration, action: () => Unit): Unit = {
    val timeline = new Timeline(new KeyFrame(Duration.millis(period.toMillis.toDouble), (_: ActionEvent) => {
      action()
    }))
    timeline.setCycleCount(Animation.INDEFINITE)
    timeline.play()
  }

}
