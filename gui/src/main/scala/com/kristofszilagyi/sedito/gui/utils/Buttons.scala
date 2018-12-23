package com.kristofszilagyi.sedito.gui.utils

import javafx.scene.control.Button

object Buttons {
  def awesome(iconChar: Char): Button = {
    val button = new Button(s"$iconChar")
    button.setStyle("""-fx-font-family: "Font Awesome 5 Free" """)
    button
  }
}