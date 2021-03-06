package com.kristofszilagyi.sedito.gui

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.kristofszilagyi.sedito.common.AssertionEx._
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps.AnyOps
import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.common._
import com.kristofszilagyi.sedito.common.utils.TupleOps.RichTuple
import com.kristofszilagyi.sedito.gui.DiffPane._
import com.kristofszilagyi.sedito.gui.JavaFxOps.scheduleOnJavaFxThread
import com.kristofszilagyi.sedito.gui.ObservableOps.RichObservable
import com.kristofszilagyi.sedito.gui.Scroller.{Aligned, LeftIsLower, NothingOnScreen, RightIsLower}
import javafx.geometry.Insets
import javafx.scene.input.{KeyCode, KeyEvent, ScrollEvent}
import javafx.scene.layout._
import javafx.scene.paint.{Color, CycleMethod, LinearGradient, Stop}
import javafx.scene.shape.StrokeLineCap
import org.fxmisc.flowless.VirtualizedScrollPane
import org.log4s.getLogger

import scala.collection.JavaConverters._
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

object DiffPane {
  private val logger = getLogger

  def offScreenY(on: LineIdx, off: LineIdx, height: Double, onY: Double): Double = {
    val diff = off.i - on.i
    onY + diff * height
  }
  private val emptyLineWidth = 3.0
  private def widen(y1: Double, y2: Double): (Double, Double) = {
    assert(y2 >= y1)
    if (y2 - y1 < 0.1) {
      (y1 - emptyLineWidth / 2, y2 + emptyLineWidth / 2)
    } else (y1, y2)
  }

  private def saveFile(maybePath: Option[Path], codeArea: Editor) = {
    maybePath match {
      case Some(path) =>
        Try(discard(Files.write(path, codeArea.getTextWithGuessedLineEnding().getBytes(StandardCharsets.UTF_8)))) match {
          case Success(_) =>
            logger.info(s"Successfully saved $path")
            Saved
          case Failure(t) =>
            logger.warn(t)(s"Failed to save $path")
            SaveFailed(t)
        }
      case None =>
        logger.info(s"No path while trying to save.")
        NoPath
    }
  }
}

final class DiffPane extends StackPane {

  //fields

  private val codeAreaLeft = new Editor(None)
  private val codeAreaRight = codeAreaLeft.otherEditor


  @SuppressWarnings(Array(Warts.Var))
  private var needToDraw: Boolean = false

  @SuppressWarnings(Array(Warts.Var))
  private var session: DiffPaneSession = DiffPaneSession.empty

  private def requestRedraw(): Unit = needToDraw = true

  private val canvas = {
    val c = new ResizableCanvas()
    c.setMouseTransparent(true)
    c
  }
  private val gc = canvas.getGraphicsContext2D

  //constructor
  {

    discard(getChildren.addAll({
      val left = new VirtualizedScrollPane(codeAreaLeft)
      val right = new VirtualizedScrollPane(codeAreaRight)
      HBox.setHgrow(left, Priority.ALWAYS)
      HBox.setHgrow(right, Priority.ALWAYS)
      val hBox = new HBox()
      hBox.setSpacing(30)
      hBox.setBackground(new Background(new BackgroundFill(Color.LIGHTGRAY, CornerRadii.EMPTY, Insets.EMPTY)))
      discard(hBox.getChildren.addAll(Seq(left, right).asJava))
      Seq(hBox, canvas).asJava
    }))

    // this should be a lot less. It was 10.ms but that resulted in freeze basically. I am not sure but I guess
    // the timeline behaviour might have changed or I just hit the critical point. The theory is that once the calculation is
    // bigger than the scheduled time it doesn't do anything else. The fix would be to have a fix wait between repeats.
    scheduleOnJavaFxThread(100.millis, () => drawEqPoints())

    this.addEventFilter(ScrollEvent.ANY, (e: ScrollEvent) => {
      val passes = 10
      (1 to passes).foreach { _ =>

        val maybeLeftVisible = Try(codeAreaLeft.lineIndicesOnScreen())
        val maybeRightVisible = Try(codeAreaRight.lineIndicesOnScreen())
        val delta = -e.getDeltaY / passes
        val (leftScroll, rightScroll) = (maybeLeftVisible, maybeRightVisible) match {
          case (Success(leftVisible), Success(rightVisible)) =>
            val topEdge = leftVisible.from.i ==== 0 || rightVisible.from.i ==== 0
            val bottomEdge = leftVisible.to.i ==== codeAreaLeft.getParagraphs.size() ||
              rightVisible.to.i ==== codeAreaRight.getParagraphs.size()
            val up = e.getDeltaY > 0
            if ((topEdge && up) || (bottomEdge && !up)) (delta, delta)
            else {
              val scrollAlignment = Scroller.calc(leftVisible,
                rightVisible, session.notMovedLines)
              scrollAlignment match {
                case Aligned | NothingOnScreen => (delta, delta)
                case LeftIsLower =>
                  if (up) (0.0, delta)
                  else (delta, 0.0)
                case RightIsLower =>
                  if (!up) (0.0, delta)
                  else (delta, 0.0)
              }
            }
          case other =>
            // :( I couldn't find a better way
            logger.info(s"Suppressing exception: $other")
            (delta, delta)
        }
        codeAreaLeft.scrollYBy(leftScroll)
        codeAreaRight.scrollYBy(rightScroll)
      }
      requestRedraw()
      e.consume()
    })

    codeAreaLeft.estimatedScrollYProperty.addChangeListener{ _ => requestRedraw() }
    codeAreaRight.estimatedScrollYProperty.addChangeListener{ _ => requestRedraw() }

    this.addEventFilter(KeyEvent.KEY_PRESSED, (e: KeyEvent) =>
      if (e.isControlDown) {
        if (e.getCode ==== KeyCode.G) {
          logger.info("Grabbing selection")
          codeAreaLeft.grabSelectionForMatch()
          codeAreaRight.grabSelectionForMatch()
          (codeAreaLeft.selectedForMatch(), codeAreaRight.selectedForMatch()) match {
            case (Some(leftSelection), Some(rightSelection)) =>
              val newMatches = session.wordAlignment.matches.filter(m => (m.left !=== leftSelection) && (m.right !=== rightSelection)) + WordMatch(leftSelection, rightSelection)()
              val newAlignment = session.wordAlignment.copy(newMatches)
              logger.info(s"Adding new match. Old size: ${session.wordAlignment.matches.size}, new size: ${newMatches.size}")
              open(FullText(codeAreaLeft.getText), FullText(codeAreaRight.getText), session.maybeLeftPath, session.maybeRightPath, newAlignment, showing = true)
            case other =>
              logger.info(s"No selection: $other")
          }
        } else if(e.getCode ==== KeyCode.H) {
          val leftSelection = codeAreaLeft.getSelection
          val rightSelection = codeAreaRight.getSelection
          val leftIndices = Wordizer.toWordIndices(codeAreaLeft.getText)
          val rightIndices = Wordizer.toWordIndices(codeAreaRight.getText)
          var leftI = leftIndices.indexWhere(_.absoluteFrom >= leftSelection.getStart)
          var rightI = rightIndices.indexWhere(_.absoluteFrom >= rightSelection.getStart)
          var assignedMatches = Vector.empty[WordMatch]
          while(leftI < leftIndices.size && rightI < rightIndices.size && leftIndices(leftI).toText ==== rightIndices(rightI).toText
                && leftIndices(leftI).absoluteFrom < leftSelection.getEnd && rightIndices(rightI).absoluteFrom < rightSelection.getEnd) {
            assignedMatches :+= WordMatch(leftIndices(leftI), rightIndices(rightI))()
            leftI += 1
            rightI += 1
          }
          val withoutNewConflict = session.wordAlignment.matches.filter(m => !assignedMatches.exists(am => am.left ==== m.left || am.right ==== m.right))
          val newAlignment = session.wordAlignment.copy(withoutNewConflict ++ assignedMatches)
          logger.info(s"Adding new matches. Old size: ${session.wordAlignment.matches.size}, new size: ${newAlignment.matches.size}")
          open(FullText(codeAreaLeft.getText), FullText(codeAreaRight.getText), session.maybeLeftPath, session.maybeRightPath, newAlignment, showing = true)
        }
      }
    )
  }

  //methods

  def testCase: TestCase = {
    //todo this is not really correct as we loose data if the original AmbiguousWordAlignment was really ambiguous
    TestCase(FullText(codeAreaLeft.getText), FullText(codeAreaRight.getText), AmbiguousWordAlignment(session.wordAlignment.matches))
  }

  private def drawEqPoints(): Unit = {
    if (needToDraw) {
      needToDraw = false
      gc.clearRect(0, 0, getWidth(), getHeight())

      val leftLinesOnScreen = codeAreaLeft.lineIndicesOnScreen()
      val rightLinesOnScreen = codeAreaRight.lineIndicesOnScreen()
      val eqPointsOnScreen = session.lineChangePoints.filter(_.overlap(leftLinesOnScreen, rightLinesOnScreen))

      val maybeFirstLeft = leftLinesOnScreen.toLines.headOption
      val maybeFirstRight = rightLinesOnScreen.toLines.headOption
      (maybeFirstLeft, maybeFirstRight).sequence.foreach { case (firstLeft, firstRight) =>
        val maybeFirstLeftBounds = codeAreaLeft.boundsInLocal(firstLeft, screenToLocal)
        val maybeFirstRightBounds = codeAreaRight.boundsInLocal(firstRight, screenToLocal)
        eqPointsOnScreen.foreach { eqPoint =>
          //todo fix this for text wrapping
          (maybeFirstLeftBounds, maybeFirstRightBounds) match {
            case (Some(firstLeftBounds), Some(firstRightBounds)) =>
              val rightOffset = 0 //todo remove?
              val leftX = firstLeftBounds.getMaxX
              val rightX = firstRightBounds.getMinX + rightOffset
              val xs = Array(leftX, leftX, rightX, rightX)

              val heightPerLine = 16.0 // todo calculate dynamically - do we need that?
              val y1 = offScreenY(firstLeft, eqPoint.left.from, heightPerLine, firstLeftBounds.getMinY)
              val y2 = offScreenY(firstLeft, eqPoint.left.to, heightPerLine, firstLeftBounds.getMinY)
              val y3 = offScreenY(firstRight, eqPoint.right.to, heightPerLine, firstRightBounds.getMinY)
              val y4 = offScreenY(firstRight, eqPoint.right.from, heightPerLine, firstRightBounds.getMinY)
              val insertColor = Color.LIGHTGREEN
              val deleteColor = Color.PINK
              gc.setFill(new LinearGradient(0.0, 0.5, 1.0, 0.5, true,
                CycleMethod.NO_CYCLE, new Stop(0.0, deleteColor), new Stop(1.0, insertColor)))
              if (eqPoint.left.size ==== 0) {
                gc.setFill(insertColor)
                gc.setStroke(insertColor)
                gc.setLineWidth(emptyLineWidth)
                gc.setLineCap(StrokeLineCap.BUTT)
                gc.strokeLine(firstLeftBounds.getMinX, y1, firstLeftBounds.getMaxX, y1)
              } else if (eqPoint.right.size ==== 0) {
                gc.setFill(deleteColor)
                gc.setStroke(deleteColor)
                gc.setLineWidth(emptyLineWidth)
                gc.setLineCap(StrokeLineCap.BUTT)
                gc.strokeLine(firstRightBounds.getMinX + rightOffset, y3, firstRightBounds.getMaxX, y3)
              }
              val (y1W, y2W) = widen(y1, y2)
              val (y4W, y3W) = widen(y4, y3)
              val ys = Array(y1W, y2W, y3W, y4W)
              gc.fillPolygon(xs, ys, 4)
              gc.setFill(Color.BLACK)
            //gc.fillRect(firstLeftBounds.getMinX, firstLeftBounds.getMinY, firstLeftBounds.getWidth, firstLeftBounds.getHeight)
            case other =>
              fail(s"Should not happen: $other")
          }
        }
      }
    }
  }

  def open(left: FullText, right: FullText, newMaybeLeftPath: Option[Path], newMaybeRightPath: Option[Path],
      newWordAlignment: UnambiguousWordAlignment, showing: Boolean) {
    needToDraw = false
    val (newSession, leftSession, rightSession) = DiffPaneSession.create(left, right , newMaybeLeftPath, newMaybeRightPath, newWordAlignment)
    session = newSession
    codeAreaLeft.setText(left, leftSession)
    codeAreaRight.setText(right, rightSession)
    updatePositionsBasedOnTracker()
    if (showing) { // just for speed
      layout()
      requestRedraw()
    }

  }

  def saveFiles(): (SaveResult, SaveResult) = {
    val leftResult = saveFile(session.maybeLeftPath, codeAreaLeft)
    val rightResult = saveFile(session.maybeRightPath, codeAreaRight)
    (leftResult, rightResult)
  }

  private def updatePositionsBasedOnTracker(): Unit = {
    val tracker = session.nextChangeTracker
    val zero = LineIdx(0)
    codeAreaLeft.moveToLine((tracker.left() - 4).max(zero))
    codeAreaRight.moveToLine((tracker.right() - 4).max(zero))
  }

  def nextChange(): Unit = {
    session.nextChangeTracker.next()
    updatePositionsBasedOnTracker()
  }
  def prevChange(): Unit = {
    session.nextChangeTracker.prev()
    updatePositionsBasedOnTracker()
  }
  def hasNextChange(): Boolean = session.nextChangeTracker.hasNext()
  def hasPrevChange(): Boolean = session.nextChangeTracker.hasPrev()

}