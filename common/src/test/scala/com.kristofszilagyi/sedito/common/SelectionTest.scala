package com.kristofszilagyi.sedito.common

import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class SelectionTest extends FreeSpecLike{

  val firstLine = "The first line"
  val secondLine = "Second line"
  val s = FullText(s"$firstLine\n$secondLine\nThird line\nAnd forth")
  ".fromAbsolute - beginning of first line" in {
    Selection.fromAbsolute(0, 1, s) shouldBe
      Selection.create(firstLine, LineIdx(0), CharIdxInLine(0), CharIdxInLine(1), 0)
  }

  ".fromAbsolute - not beginning of first line" in {
    Selection.fromAbsolute(2, 3, s) shouldBe
      Selection.create(firstLine, LineIdx(0), CharIdxInLine(2), CharIdxInLine(3), 2)
  }

  ".fromAbsolute - beginning of second line" in {
    val secondLineFirstCharIdx = firstLine.length + 1
    Selection.fromAbsolute(secondLineFirstCharIdx, secondLineFirstCharIdx + 1, s) shouldBe
      Selection.create(secondLine, LineIdx(1), CharIdxInLine(0), CharIdxInLine(1), secondLineFirstCharIdx)
  }

  ".fromAbsolute - not beginning of second line" in {
    val secondLineFirstCharIdx = firstLine.length + 1

    Selection.fromAbsolute(secondLineFirstCharIdx + 2, secondLineFirstCharIdx + 3, s) shouldBe
      Selection.create(secondLine, LineIdx(1), CharIdxInLine(2), CharIdxInLine(3), secondLineFirstCharIdx + 2)
  }
}
