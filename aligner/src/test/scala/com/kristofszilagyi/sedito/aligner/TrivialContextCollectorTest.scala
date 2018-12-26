package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import com.kristofszilagyi.sedito.common.{FullText, Selection, UnambiguousWordAlignment, WordMatch}
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

class TrivialContextCollectorTest extends FreeSpecLike {
  "empty" in {
    TrivialContextCorrector.correct(FullText(""), FullText(""), UnambiguousWordAlignment(Set.empty)) shouldBe
      UnambiguousWordAlignment(Set.empty)
  }

  "empty alignment" in {
    TrivialContextCorrector.correct(FullText("something here"), FullText("nothing there"), UnambiguousWordAlignment(Set.empty)) shouldBe
      UnambiguousWordAlignment(Set.empty)
  }

  "forward edge" in {
    val left = FullText("<include,")
    val right = FullText("<include;")
    val m1 = WordMatch(
      Selection.fromAbsolute(1, 8, left).getAssert,
      Selection.fromAbsolute(1, 8, right).getAssert
    )()
    val m2 = WordMatch(
      Selection.fromAbsolute(0, 1, left).getAssert,
      Selection.fromAbsolute(0, 1, right).getAssert
    )()
    TrivialContextCorrector.correct(left, right, UnambiguousWordAlignment(Set.empty)) shouldBe
      UnambiguousWordAlignment(Set(m1, m2))
  }

  "forward middle" in {
    val left = FullText(".com.sun")
    val right = FullText(",com.oracle")
    val m1 = WordMatch(
      Selection.fromAbsolute(1, 4, left).getAssert,
      Selection.fromAbsolute(1, 4, right).getAssert
    )()
    val m2 = WordMatch(
      Selection.fromAbsolute(4, 5, left).getAssert,
      Selection.fromAbsolute(4, 5, right).getAssert
    )()
    TrivialContextCorrector.correct(left, right, UnambiguousWordAlignment(Set(m1))) shouldBe
      UnambiguousWordAlignment(Set(m1, m2))
  }

  "backward edge" in {
    val left = FullText(",include>")
    val right = FullText("/include>")
    val m1 = WordMatch(
      Selection.fromAbsolute(1, 8, left).getAssert,
      Selection.fromAbsolute(1, 8, right).getAssert
    )()
    val m2 = WordMatch(
      Selection.fromAbsolute(8, 9, left).getAssert,
      Selection.fromAbsolute(8, 9, right).getAssert
    )()
    TrivialContextCorrector.correct(left, right, UnambiguousWordAlignment(Set.empty)) shouldBe
      UnambiguousWordAlignment(Set(m1, m2))
  }

  "backward middle" in {
    val left = FullText(".com.sun")
    val right = FullText(",com.oracle")
    val m1 = WordMatch(
      Selection.fromAbsolute(4, 5, left).getAssert,
      Selection.fromAbsolute(4, 5, right).getAssert
    )()
    val m2 = WordMatch(
      Selection.fromAbsolute(1, 4, left).getAssert,
      Selection.fromAbsolute(1, 4, right).getAssert
    )()
    TrivialContextCorrector.correct(left, right, UnambiguousWordAlignment(Set(m1))) shouldBe
      UnambiguousWordAlignment(Set(m1, m2))
  }

  "conflicting with an original" in {
    val left = FullText(".com..")
    val right = FullText(",com..")
    val m1 = WordMatch(
      Selection.fromAbsolute(1, 4, left).getAssert,
      Selection.fromAbsolute(1, 4, right).getAssert
    )()
    val m2 = WordMatch(
      Selection.fromAbsolute(4, 5, left).getAssert,
      Selection.fromAbsolute(5, 6, right).getAssert
    )()

    TrivialContextCorrector.correct(left, right, UnambiguousWordAlignment(Set(m1, m2))) shouldBe
      UnambiguousWordAlignment(Set(m1, m2))
  }

  "forward and backward conflict" in {
    val left = FullText(".com..alma")
    val right = FullText(",com.alma")
    val m1 = WordMatch(
      Selection.fromAbsolute(1, 4, left).getAssert,
      Selection.fromAbsolute(1, 4, right).getAssert
    )()
    val m2 = WordMatch(
      Selection.fromAbsolute(6, 10, left).getAssert,
      Selection.fromAbsolute(5, 9, right).getAssert
    )()
    val m3 = WordMatch(
      Selection.fromAbsolute(4, 5, left).getAssert,
      Selection.fromAbsolute(4, 5, right).getAssert
    )()

    TrivialContextCorrector.correct(left, right, UnambiguousWordAlignment(Set(m1, m2))) shouldBe
      UnambiguousWordAlignment(Set(m1, m2, m3))
  }

  // this whole things perform poorly on the following examples:
  /*
    reset()
    session = newSession
    session.editTypes.foreach{ case (lineIdx, edits) => applyLineTypeCssOnLineNumber(lineIdx, Some(edits))}
    applyLineEdits(fullText)
    applyCharEdits()
    moveTo(math.max(firstChangeLine.i - 4, 0), 0)
    requestFollowCaret()
  }

   private def applyLineTypeCssOnLineNumber(lineIdx: LineIdx, editType: Option[LineEdits]): Unit = {

  compared to

  def setText(fullText: FullText, newSession: EditorSession): Unit = {
    reset()
    session = newSession
    session.editTypes.foreach{ case (lineIdx, edits) => applyLineTypeCssOnLineNumber(lineIdx, Some(edits))}
    applyLineEdits(fullText)
    applyCharEdits()
  }

  def moveToLine(line: LineIdx): Unit = {
    moveTo(line.i, 0)
    showParagraphAtTop(line.i)
  }

  private def applyLineTypeCssOnLineNumber(lineIdx: LineIdx, editType: Option[LineEdits]): Unit = {

  it starts from the behind and assumes the last } and ) is the same while they are not


  OR here:

  codeAreaRight.setText(right, rightSession, rightFirstChangeLine)
  if (showing) { // just for speed
    layout()
    requestRedraw()
  }

  vs

  updatePositionsBasedOnTracker()
  if (showing) { // just for speed
    layout()
    requestRedraw()
  }

  it assumes the first line's ) is the same

  see commit c09a73d6d8b6a48277bd1903eea8d41d17e927a4 for these example
  and 4139283a201b87b63be1d5cd08e7fc3e8bc85ab6 for more example
   */
  //spaces?
}