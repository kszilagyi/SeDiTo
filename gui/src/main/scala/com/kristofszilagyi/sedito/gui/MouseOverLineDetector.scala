package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.{LineIdx, Warts}
import javafx.scene.input.MouseEvent

import scala.compat.java8.OptionConverters.RichOptionalGeneric
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import scala.concurrent.duration.DurationInt
final class MouseOverLineDetector(editor: Editor, onEnter: LineIdx => Unit, onExit: () => Unit) {
  @SuppressWarnings(Array(Warts.Var))
  private var maybePreviousLine: Option[LineIdx] = None

  @SuppressWarnings(Array(Warts.Var))
  private var moveEvent: Option[MouseEvent] = None

  editor.addEventHandler(MouseEvent.MOUSE_MOVED, (e: MouseEvent) => {
    moveEvent = Some(e)
  })
  editor.addEventHandler(MouseEvent.MOUSE_EXITED, (e: MouseEvent) => {
    onExit()
    moveEvent = None
    maybePreviousLine = None
  })
  JavaFxOps.scheduleOnJavaFxThread(10.millis, { () =>
    moveEvent.foreach { e =>
      val bounds = editor.lineIndicesOnScreen().toLines.map(l => l -> editor.getParagraphBoundsOnScreen(l.i))
      val maybeCurrentLine = bounds.find(b => b._2.asScala.exists(bound => bound.getMinY > e.getScreenY)).map(_._1 - 1)
      maybeCurrentLine.foreach { currentLine =>
        maybePreviousLine.foreach { previousLine =>
          if (previousLine !=== currentLine) {
            onExit()
          }
        }
        maybePreviousLine = Some(currentLine)
        onEnter(currentLine)
      }
    }
    moveEvent = None
  })


}
