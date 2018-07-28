package com.kristofszilagyi.sedito.gui

import javafx.beans.value.{ChangeListener, ObservableValue}

object ObservableOps {
  implicit class RichObservable[T](value: ObservableValue[T]) {
    def addChangeListener(f: T => Unit): Unit = {
      value.addListener(new ChangeListener[T] {
        def changed(observableValue: ObservableValue[_ <: T], oldValue: T, newValue: T): Unit =
          f(newValue)
      })
    }
  }
}
