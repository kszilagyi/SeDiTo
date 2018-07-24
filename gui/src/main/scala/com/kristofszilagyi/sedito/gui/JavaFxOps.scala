package com.kristofszilagyi.sedito.gui

import javafx.event.ActionEvent
import javafx.scene.control.MenuItem

object JavaFxOps {
  def menuItem(title: String, action: ActionEvent => Unit): MenuItem = {
    val i = new MenuItem(title)
    i.setOnAction(action(_))
    i
  }
}
