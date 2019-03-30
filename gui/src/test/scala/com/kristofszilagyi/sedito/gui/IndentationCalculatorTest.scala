package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.LineIdx
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class IndentationCalculatorTest extends FreeSpecLike {
  "all 0" in {
    val leftLines = 30
    val rightLines = 40
    val leftIdx = LineIdx(10)
    val rightIdx = LineIdx(15)
    val indent = new IndentationCalculator(Vector.fill(leftLines)("import"), Vector.fill(rightLines)("import")).calcIndentation(leftIdx, rightIdx)
    indent shouldBe Indentation(
      IndentationOneSide(Seq.fill(7)(0), 0, Seq.fill(7)(0), leftIdx, 19),
      IndentationOneSide(Seq.fill(7)(0), 0, Seq.fill(7)(0), rightIdx, 24),
    )
  }
}
