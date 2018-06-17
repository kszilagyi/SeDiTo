package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps.AnyOps
import com.kristofszilagyi.sedito.common._
import javafx.animation.{KeyFrame, Timeline}
import javafx.scene.input.{KeyCode, KeyEvent, ScrollEvent}
import org.log4s.getLogger
import scalafx.Includes.jfxRegion2sfx
import scalafx.scene.layout.{HBox, Priority}

final class DiffPane extends HBox {
  private val logger = getLogger

  private val codeAreaLeft = PaddableEditor.test()
  private val codeAreaRight = PaddableEditor.test()
  @SuppressWarnings(Array(Warts.Var))
  private var wordAlignment: UnambiguousWordAlignment = UnambiguousWordAlignment(Set.empty)
  def testCase: TestCase = {
    //todo this is not really correct as we loose data if the original AmbiguousWordAlignment was really ambiguous
    TestCase(FullText(codeAreaLeft.getText), FullText(codeAreaRight.getText), AmbiguousWordAlignment(wordAlignment.matches))
  }

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

    def applyHighlight(editor: PaddableEditor, highlight: Map[LineIdx, Traversable[CharEdit]]): Unit = {
      highlight.foreach { case (line, edits) =>
        edits.foreach { edit =>
          editor.setCharEdit(line, edit.from, edit.to, edit.editType)
        }
      }
    }

    val highlight = CharHighlightCalculator.calc(leftLines, rightLines, newWordAlignment, lineAlignment)
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
