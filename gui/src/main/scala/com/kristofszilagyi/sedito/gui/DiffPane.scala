package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.AssertionEx._
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps.AnyOps
import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.common._
import com.kristofszilagyi.sedito.common.utils.TupleOps.RichTuple
import com.kristofszilagyi.sedito.gui.DiffPane._
import com.kristofszilagyi.sedito.gui.ObservableOps.RichObservable
import com.kristofszilagyi.sedito.gui.Scroller.{Aligned, LeftIsLower, NothingOnScreen, RightIsLower}
import javafx.animation.{Animation, KeyFrame, Timeline}
import javafx.event.ActionEvent
import javafx.scene.input.{KeyCode, KeyEvent, ScrollEvent}
import javafx.scene.layout.{HBox, Priority, StackPane}
import javafx.scene.paint.{Color, CycleMethod, LinearGradient, Stop}
import javafx.scene.shape.StrokeLineCap
import javafx.util.Duration
import org.fxmisc.flowless.VirtualizedScrollPane
import org.log4s.getLogger

import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap

object DiffPane {
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
}

final class DiffPane extends StackPane {
  //fields
  private val logger = getLogger

  private val codeAreaLeft = new Editor()
  private val codeAreaRight = new Editor()
  @SuppressWarnings(Array(Warts.Var))
  private var wordAlignment: UnambiguousWordAlignment = UnambiguousWordAlignment(Set.empty)
  @SuppressWarnings(Array(Warts.Var))
  private var eqPoints: Traversable[EquivalencePoint] = Traversable.empty

  @SuppressWarnings(Array(Warts.Var))
  private var needToDraw: Boolean = false

  @SuppressWarnings(Array(Warts.Var))
  private var notMovedLines: TreeMap[LineIdx, LineIdx] = TreeMap.empty

  private def requestRedraw(): Unit = needToDraw = true

  private val canvas = {
    val c = new ResizableCanvas()
    c.setMouseTransparent(true)
    c
  }
  private val gc = canvas.getGraphicsContext2D

  //constructor
  {
    codeAreaLeft.setOther(codeAreaRight)
    codeAreaRight.setOther(codeAreaLeft)

    discard(getChildren.addAll({
      val left = new VirtualizedScrollPane(codeAreaLeft)
      val right = new VirtualizedScrollPane(codeAreaRight)
      HBox.setHgrow(left, Priority.ALWAYS)
      HBox.setHgrow(right, Priority.ALWAYS)
      val hBox = new HBox()
      hBox.setSpacing(10)
      discard(hBox.getChildren.addAll(Seq(left, right).asJava))
      Seq(hBox, canvas).asJava
    }))

    val refreshLoop = new Timeline(new KeyFrame(Duration.millis(10), (_: ActionEvent) => {
      drawEqPoints()
    }))
    refreshLoop.setCycleCount(Animation.INDEFINITE)
    refreshLoop.play()

    this.addEventFilter(ScrollEvent.ANY, (e: ScrollEvent) => {
      val scrollAlignment = Scroller.calc(codeAreaLeft.lineIndicesOnScreen(),
        codeAreaRight.lineIndicesOnScreen(), notMovedLines)

      val down = e.getDeltaY > 0
      val delta = -e.getDeltaY
      val (leftScroll, rightScroll) = scrollAlignment match {
        case Aligned | NothingOnScreen => (delta, delta)
        case LeftIsLower =>
          if (down) (0.0, delta)
          else (delta, 0.0)
        case RightIsLower =>
          if (!down) (0.0, delta)
          else (delta, 0.0)
      }
      codeAreaLeft.scrollYBy(leftScroll)
      codeAreaRight.scrollYBy(rightScroll)
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
              val newMatches = wordAlignment.matches.filter(m => (m.left !=== leftSelection) && (m.right !=== rightSelection)) + WordMatch(leftSelection, rightSelection)
              val newAlignment = wordAlignment.copy(newMatches)
              logger.info(s"Adding new match. Old size: ${wordAlignment.matches.size}, new size: ${newMatches.size}")
              openTestCase(FullText(codeAreaLeft.getText), FullText(codeAreaRight.getText), newAlignment)
            case other =>
              logger.info(s"No selection: $other")
          }
        }
      }
    )
  }

  //methods

  def testCase: TestCase = {
    //todo this is not really correct as we loose data if the original AmbiguousWordAlignment was really ambiguous
    TestCase(FullText(codeAreaLeft.getText), FullText(codeAreaRight.getText), AmbiguousWordAlignment(wordAlignment.matches))
  }

  private def drawEqPoints(): Unit = {
    if (needToDraw) {
      needToDraw = false
      gc.clearRect(0, 0, getWidth(), getHeight())

      val leftLinesOnScreen = codeAreaLeft.lineIndicesOnScreen()
      val rightLinesOnScreen = codeAreaRight.lineIndicesOnScreen()
      val eqPointsOnScreen = eqPoints.filter(_.overlap(leftLinesOnScreen, rightLinesOnScreen))

      val maybeFirstLeft = leftLinesOnScreen.toLines.headOption
      val maybeFirstRight = rightLinesOnScreen.toLines.headOption
      (maybeFirstLeft, maybeFirstRight).sequence.foreach { case (firstLeft, firstRight) =>
        eqPointsOnScreen.foreach { eqPoint =>
          //todo fix this for text wrapping
          val maybeFirstLeftBounds = codeAreaLeft.boundsInLocal(firstLeft, screenToLocal)
          val maybeFirstRightBounds = codeAreaRight.boundsInLocal(firstRight, screenToLocal)
          (maybeFirstLeftBounds, maybeFirstRightBounds) match {
            case (Some(firstLeftBounds), Some(firstRightBounds)) =>
              val rightOffset = 35
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
            case other => fail(s"Should not happen: $other")
          }
        }
      }
    }
  }

  private def getParagraphTexts(codeArea: Editor): Lines ={
    val size = codeArea.getParagraphs.size()
    Lines((0 until size).map(i => codeArea.getText(i)))
  }

  def openTestCase(left: FullText, right: FullText, newWordAlignment: UnambiguousWordAlignment): Unit = {
    needToDraw = false
    //todo probably reset should recreate everything
    codeAreaRight.reset()
    codeAreaLeft.reset()
    codeAreaLeft.replaceText(left.s)
    codeAreaRight.replaceText(right.s)

    codeAreaLeft.moveTo(0)
    codeAreaLeft.requestFollowCaret()
    codeAreaRight.moveTo(0)
    codeAreaRight.requestFollowCaret()
    wordAlignment = newWordAlignment
    val leftLines = getParagraphTexts(codeAreaLeft)
    val rightLines = getParagraphTexts(codeAreaRight)

    val lineAlignment = WhiteSpaceAligner.align(leftLines, rightLines, LineAligner.align(newWordAlignment))

    val deleted = (0 until codeAreaLeft.getParagraphs.size()).map(LineIdx.apply).filterNot(l => lineAlignment.matches.map(_.leftLineIdx).contains(l))
    val inserted = (0 until codeAreaRight.getParagraphs.size()).map(LineIdx.apply).filterNot(l => lineAlignment.matches.map(_.rightLineIdx).contains(l))
    val partitioned = lineAlignment.partition
    val moved = partitioned.moved
    notMovedLines = TreeMap(partitioned.notMoved.flatMap(LineMatch.unapply).toSeq: _*)
    val notMovedLeft = partitioned.notMoved.map(_.leftLineIdx)
    val notMovedRight = partitioned.notMoved.map(_.rightLineIdx)

    deleted.foreach(l => codeAreaLeft.setLineType(l, LineDeleted))
    inserted.foreach(l => codeAreaRight.setLineType(l, LineInserted))
    moved.foreach(m => codeAreaLeft.setLineType(m.leftLineIdx, LineMoved(m.rightLineIdx)))
    moved.foreach(m => codeAreaRight.setLineType(m.rightLineIdx, LineMoved(m.leftLineIdx)))
    notMovedLeft.foreach(l => codeAreaLeft.setLineType(l, LineSame))
    notMovedRight.foreach(l => codeAreaRight.setLineType(l, LineSame))

    def applyHighlight(editor: Editor, highlight: Map[LineIdx, Traversable[CharEdit]]): Unit = {
      highlight.foreach { case (line, edits) =>
        edits.foreach { edit =>
          editor.setCharEdit(line, edit.from, edit.to, edit.editType)
        }
      }
    }
    eqPoints = InsertionPointCalculator.calc(partitioned.notMoved, moved, leftLineCount = leftLines.l.size,
      rightLineCount = rightLines.l.size)
    val highlight = CharHighlightCalculator.calc(leftLines, rightLines, newWordAlignment, lineAlignment)
    applyHighlight(codeAreaLeft, highlight.left)
    applyHighlight(codeAreaRight, highlight.right)
    layout()
    requestRedraw()
  }
}
