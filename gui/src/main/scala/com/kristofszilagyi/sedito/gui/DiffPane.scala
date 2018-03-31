package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common._
import javafx.animation.{KeyFrame, Timeline}
import javafx.scene.input.ScrollEvent
import scalafx.Includes.jfxRegion2sfx
import scalafx.scene.layout.{HBox, Priority}

final class DiffPane extends HBox {
  private val codeAreaLeft = PaddableEditor.test()
  private val codeAreaRight = PaddableEditor.test()
  codeAreaLeft.setOther(codeAreaRight)
  codeAreaRight.setOther(codeAreaLeft)
  spacing = 10
  children = Seq(jfxRegion2sfx(codeAreaLeft), jfxRegion2sfx(codeAreaRight))

  HBox.setHgrow(codeAreaLeft, Priority.Always)
  HBox.setHgrow(codeAreaRight, Priority.Always)
  this.addEventFilter(ScrollEvent.ANY, (e: ScrollEvent) => {
    codeAreaLeft.scrollYBy(-e.getDeltaY)
    codeAreaRight.scrollYBy(-e.getDeltaY)
    codeAreaLeft.applyAllPadding()
    codeAreaRight.applyAllPadding()
    e.consume()
  })

  private def getParagraphTexts(codeArea: PaddableEditor): Lines ={
    val size = codeArea.getParagraphs.size()
    Lines((0 until size).map(i => codeArea.getText(i)))
  }

  def openTestCase(left: String, right: String, oldAlignment: LineAlignment): Unit = {
    //todo probably reset should recreate everything
    codeAreaRight.reset()
    codeAreaLeft.reset()
    codeAreaLeft.replaceText(left)
    codeAreaRight.replaceText(right)

    val leftLines = getParagraphTexts(codeAreaLeft)
    val rightLines = getParagraphTexts(codeAreaRight)

    val wordAlignment = WordAlignment.fromOld(leftLines, rightLines, oldAlignment)
    val lineAlignment = WhiteSpaceAligner.align(leftLines, rightLines, LineAligner.align(wordAlignment))

    val deleted = (0 until codeAreaLeft.getParagraphs.size()).map(LineIdx.apply).filterNot(l => lineAlignment.matches.map(_.leftLineIdx).contains(l))
    val inserted = (0 until codeAreaRight.getParagraphs.size()).map(LineIdx.apply).filterNot(l => lineAlignment.matches.map(_.rightLineIdx).contains(l))
    val partitioned = lineAlignment.partition
    val moved = partitioned.moved
    val notMovedLeft = partitioned.notMoved.map(_.leftLineIdx)
    val notMovedRight = partitioned.notMoved.map(_.rightLineIdx)

    deleted.foreach(l => codeAreaLeft.setLineType(l, Deleted))
    inserted.foreach(l => codeAreaRight.setLineType(l, Inserted))
    moved.foreach(m => codeAreaLeft.setLineType(m.leftLineIdx, Moved(m.rightLineIdx)))
    moved.foreach(m => codeAreaRight.setLineType(m.rightLineIdx, Moved(m.leftLineIdx)))
    notMovedLeft.foreach(l => codeAreaLeft.setLineType(l, Same))
    notMovedRight.foreach(l => codeAreaRight.setLineType(l, Same))

    def applyHighlight(codeArea: SCodeArea, highlight: Map[LineIdx, Traversable[CharEdit]]): Unit = {
      highlight.foreach { case (line, edits) =>
        edits.foreach { edit =>
          codeAreaLeft.setCharEdit(line, edit.from, edit.to, edit.editType)
        }
      }
    }

    val highlight = CharHighlightCalculator.calc(leftLines, rightLines, wordAlignment, lineAlignment)
    applyHighlight(codeAreaLeft, highlight.left)
    applyHighlight(codeAreaRight, highlight.right)

    PaddingCalculator.calc(partitioned.notMoved,
      LineIdx(codeAreaLeft.getParagraphs.size - 1),
      LineIdx(codeAreaRight.getParagraphs.size - 1)
    ).foreach { padding =>
      val editor = padding.side match {
        case Left => codeAreaLeft
        case Right => codeAreaRight
      }
      editor.setLinePadding(padding.line, padding.amount)
    }

    //hack to make sure padding works
    import javafx.util.Duration
    val timeline = new Timeline(new KeyFrame(Duration.millis(100), _ => {
      codeAreaLeft.applyAllPadding()
      codeAreaRight.applyAllPadding()
    }))
    timeline.play()
  }
}
