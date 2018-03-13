package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common._
import javafx.animation.{KeyFrame, Timeline}
import org.bitbucket.cowwoc.diffmatchpatch.DiffMatchPatch
import org.bitbucket.cowwoc.diffmatchpatch.DiffMatchPatch.Operation
import scalafx.scene.layout.{HBox, Priority}
import TypeSafeEqualsOps._
import scala.collection.JavaConverters._
import scalafx.Includes._

final class DiffPane extends HBox {
  private val codeAreaLeft = PaddableEditor.test()
  private val codeAreaRight = PaddableEditor.test()
  codeAreaLeft.setOther(codeAreaRight)
  codeAreaRight.setOther(codeAreaLeft)
  spacing = 10
  children = Seq(jfxRegion2sfx(codeAreaLeft), jfxRegion2sfx(codeAreaRight))

  HBox.setHgrow(codeAreaLeft, Priority.Always)
  HBox.setHgrow(codeAreaRight, Priority.Always)

  def openTestCase(left: String, right: String, alignment: Alignment): Unit = {
    //todo probably reset should recreate everything
    codeAreaRight.reset()
    codeAreaLeft.reset()
    codeAreaLeft.replaceText(left)
    codeAreaRight.replaceText(right)
    val deleted = (0 until codeAreaLeft.getParagraphs.size()).map(LineIdx.apply).filterNot(l => alignment.matches.map(_.leftLineIdx).contains(l))
    val inserted = (0 until codeAreaRight.getParagraphs.size()).map(LineIdx.apply).filterNot(l => alignment.matches.map(_.rightLineIdx).contains(l))
    val partitioned = alignment.partition
    val moved = partitioned.moved
    val notMovedLeft = partitioned.notMoved.map(_.leftLineIdx)
    val notMovedRight = partitioned.notMoved.map(_.rightLineIdx)
    deleted.foreach(l => codeAreaLeft.setLineType(l, Deleted))
    inserted.foreach(l => codeAreaRight.setLineType(l, Inserted))
    moved.foreach(m => codeAreaLeft.setLineType(m.leftLineIdx, Moved(m.rightLineIdx)))
    moved.foreach(m => codeAreaRight.setLineType(m.rightLineIdx, Moved(m.leftLineIdx)))
    notMovedLeft.foreach(l => codeAreaLeft.setLineType(l, Same))
    notMovedRight.foreach(l => codeAreaRight.setLineType(l, Same))
    alignment.matches.foreach { m =>
      val leftLine = codeAreaLeft.getParagraph(m.leftLineIdx.i).getText
      val rightLine = codeAreaRight.getParagraph(m.rightLineIdx.i).getText
      val differ = new DiffMatchPatch()
      val inlineDiff = differ.diffMain(leftLine, rightLine)
      differ.diffCleanupSemantic(inlineDiff)

      val leftDiffs = inlineDiff.asScala.filter(d => d.operation ==== Operation.DELETE || d.operation ==== Operation.EQUAL)
      val rightDiffs = inlineDiff.asScala.filter(d => d.operation ==== Operation.INSERT || d.operation ==== Operation.EQUAL)
      final case class PosDiff(from: CharIdxInLine, to: CharIdxInLine, op: Operation)
      def toPositions(diffs: Seq[DiffMatchPatch.Diff]) = {
        diffs.foldLeft(Seq.empty[PosDiff]) { case (result, diff) =>
          val op = diff.operation
          val len = diff.text.length
          val lastPos = result.lastOption.map(_.to).getOrElse(CharIdxInLine(0))
          val to = lastPos + len
          result :+ PosDiff(from = lastPos, to = to, op = op)
        }
      }

      toPositions(leftDiffs).foreach { d =>
        codeAreaLeft.setCharCss(m.leftLineIdx, d.from, d.to, EditType.from(d.op))
      }
      toPositions(rightDiffs).foreach { d =>
        codeAreaRight.setCharCss(m.rightLineIdx, d.from, d.to, EditType.from(d.op))
      }
    }
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
