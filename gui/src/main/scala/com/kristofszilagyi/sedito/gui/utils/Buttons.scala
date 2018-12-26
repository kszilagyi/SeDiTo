package com.kristofszilagyi.sedito.gui.utils


import javafx.event.ActionEvent
import javafx.scene.control.Button

object Buttons {
  def awesome(iconChar: Char, handler: ActionEvent => Unit): Button = {
    val button = new Button(s"$iconChar")
    button.setStyle("""-fx-font-family: "Font Awesome 5 Free" """)
    button.setOnAction{ e => handler(e) }
    button
  }
}