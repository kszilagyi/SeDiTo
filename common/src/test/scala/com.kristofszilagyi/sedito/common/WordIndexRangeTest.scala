package com.kristofszilagyi.sedito.common

import cats.data.Validated.Invalid
import com.kristofszilagyi.sedito.common.testutils.ValidatedTestOps.RichValidated
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class WordIndexRangeTest extends FreeSpecLike{

  val firstLine = "The first line"
  val secondLine = "Second line"
  val s = s"$firstLine\n$secondLine\nThird line\nAnd forth"
  ".toSelection - beginning of first line" in {
    WordIndexRange.create(0, 1, s).get.toSelection shouldBe
      Selection.create(firstLine, LineIdx(0), CharIdxInLine(0), CharIdxInLine(1)).get
  }

  ".toSelection - not beginning of first line" in {
    WordIndexRange.create(2, 3, s).get.toSelection shouldBe
      Selection.create(firstLine, LineIdx(0), CharIdxInLine(2), CharIdxInLine(3)).get
  }

  ".toSelection - beginning of second line" in {
    val secondLineFirstCharIdx = firstLine.length + 1
    WordIndexRange.create(secondLineFirstCharIdx, secondLineFirstCharIdx + 1, s).get.toSelection shouldBe
      Selection.create(secondLine, LineIdx(1), CharIdxInLine(0), CharIdxInLine(1)).get
  }

  ".toSelection - not beginning of second line" in {
    val secondLineFirstCharIdx = firstLine.length + 1

    WordIndexRange.create(secondLineFirstCharIdx + 2, secondLineFirstCharIdx + 3, s).get.toSelection shouldBe
      Selection.create(secondLine, LineIdx(1), CharIdxInLine(2), CharIdxInLine(3)).get
  }


  "can't create WordIndexRange with spaces" in {
    val word = " "
    WordIndexRange.create(0, 1, word) shouldBe Invalid(NotAWord(0, 1, word))
  }

  "can't create WordIndexRange with \n" in {
    val word = "\n"
    WordIndexRange.create(0, 1, word) shouldBe Invalid(NotAWord(0, 1, word))
  }
}
