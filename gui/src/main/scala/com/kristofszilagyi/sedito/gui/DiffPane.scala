package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.AssertionEx._
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps.AnyOps
import com.kristofszilagyi.sedito.common.Warts.discard
import com.kristofszilagyi.sedito.common._
import javafx.scene.input.{KeyCode, KeyEvent, ScrollEvent}
import javafx.scene.layout.{HBox, Priority, StackPane}
import org.fxmisc.flowless.VirtualizedScrollPane
import org.log4s.getLogger

import scala.collection.JavaConverters._
import DiffPane._

object DiffPane {
  def offScreenY(on: LineIdx, off: LineIdx, height: Double, onY: Double): Double = {
    val diff = off.i - on.i
    onY + diff * height
  }
}

final class DiffPane extends StackPane {
  private val logger = getLogger

  private val codeAreaLeft = new Editor()
  private val codeAreaRight = new Editor()
  @SuppressWarnings(Array(Warts.Var))
  private var wordAlignment: UnambiguousWordAlignment = UnambiguousWordAlignment(Set.empty)
  @SuppressWarnings(Array(Warts.Var))
  private var eqPoints: Traversable[EquivalencePoint] = Traversable.empty
  def testCase: TestCase = {
    //todo this is not really correct as we loose data if the original AmbiguousWordAlignment was really ambiguous
    TestCase(FullText(codeAreaLeft.getText), FullText(codeAreaRight.getText), AmbiguousWordAlignment(wordAlignment.matches))
  }

  codeAreaLeft.setOther(codeAreaRight)
  codeAreaRight.setOther(codeAreaLeft)
  private val canvas = {
    val c = new ResizableCanvas()
    c.setMouseTransparent(true)
    c
  }
  private val gc = canvas.getGraphicsContext2D

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

  private def drawEqPoints(): Unit = {
    val leftLinesOnScreen = codeAreaLeft.lineIndicesOnScreen()
    val rightLinesOnScreen = codeAreaRight.lineIndicesOnScreen()
    val eqPointsOnScreen = eqPoints.map(e => (e.intersect(leftLinesOnScreen, rightLinesOnScreen), e))
    gc.clearRect(0, 0, getWidth(), getHeight())

    eqPointsOnScreen.foreach{ case (visible, original) =>
      visible match {
        case Some(vis) =>
          val maybeLeftFromOnScreen = codeAreaLeft.boundsInLocal(vis.left.from, screenToLocal)
          val maybeLeftToOnScreen = codeAreaLeft.boundsInLocal(vis.left.to - 1, screenToLocal)

          val maybeRightFromOnScreen = codeAreaRight.boundsInLocal(vis.right.from, screenToLocal)
          val maybeRightToOnScreen = codeAreaRight.boundsInLocal(vis.right.to - 1, screenToLocal)

          (maybeLeftFromOnScreen, maybeLeftToOnScreen, maybeRightFromOnScreen, maybeRightToOnScreen) match {
            case (Some(leftFromOnScreen), Some(leftToOnScreen), Some(rightFromOnScreen), Some(rightToOnScreen)) =>
              val rightOffset = 35
              val leftX = leftFromOnScreen.getMaxX
              val rightX = rightFromOnScreen.getMinX + rightOffset
              val xs = Array(leftX, leftX, rightX, rightX)

              val ys = {
                val heightPerLine = if (vis.left.size > 0) (leftToOnScreen.getMinY - leftFromOnScreen.getMinY) / vis.left.size
                  else if(vis.right.size > 0) (rightToOnScreen.getMinY - rightFromOnScreen.getMinY) / vis.right.size
                  else fail(s"both side is zero, doesn't make sense: $vis")

                val y1 = offScreenY(vis.left.from, original.left.from, heightPerLine, leftFromOnScreen.getMinY)
                val y2 = offScreenY(vis.left.to, original.left.to, heightPerLine, leftToOnScreen.getMaxY)
                val y3 = offScreenY(vis.right.to, original.right.to, heightPerLine, rightToOnScreen.getMaxY)
                val y4 = offScreenY(vis.right.from, original.right.from, heightPerLine, rightFromOnScreen.getMinY)
                Array(y1, y2, y3, y4)
              }
              gc.fillPolygon(xs, ys, 4)
            case other => fail(s"Should not happen: $other")
          }
        case None => //not visible
          //todo figure out if some part is visible or not
      }
    }
  }
  this.addEventFilter(ScrollEvent.ANY, (e: ScrollEvent) => {
    codeAreaLeft.scrollYBy(-e.getDeltaY)
    codeAreaRight.scrollYBy(-e.getDeltaY)
    drawEqPoints()
    e.consume()
  })

  private def getParagraphTexts(codeArea: Editor): Lines ={
    val size = codeArea.getParagraphs.size()
    Lines((0 until size).map(i => codeArea.getText(i)))
  }
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

  def openTestCase(left: FullText, right: FullText, newWordAlignment: UnambiguousWordAlignment): Unit = {
    //todo probably reset should recreate everything
    codeAreaRight.reset()
    codeAreaLeft.reset()
    codeAreaLeft.replaceText(left.s)
    codeAreaRight.replaceText(right.s)
    wordAlignment = newWordAlignment
    val leftLines = getParagraphTexts(codeAreaLeft)
    val rightLines = getParagraphTexts(codeAreaRight)

    val lineAlignment = WhiteSpaceAligner.align(leftLines, rightLines, LineAligner.align(newWordAlignment))

    val deleted = (0 until codeAreaLeft.getParagraphs.size()).map(LineIdx.apply).filterNot(l => lineAlignment.matches.map(_.leftLineIdx).contains(l))
    val inserted = (0 until codeAreaRight.getParagraphs.size()).map(LineIdx.apply).filterNot(l => lineAlignment.matches.map(_.rightLineIdx).contains(l))
    val partitioned = lineAlignment.partition
    val moved = partitioned.moved
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
    // drawEqPoints() -> crashes :(


  }
}
