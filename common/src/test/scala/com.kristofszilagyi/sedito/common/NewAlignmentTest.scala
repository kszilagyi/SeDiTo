package com.kristofszilagyi.sedito.common

import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class NewAlignmentTest extends FreeSpecLike{
  val alignment = Alignment(Set(Match(LineIdx(0), LineIdx(0))))

  private def m(left: String, right: String, leftStart: Int, leftEnd: Int, rightStart: Int, rightEnd: Int): NewMatch = {
    NewMatch(
      Selection(left, LineIdx(0), from = CharIdxInLine(leftStart), toExcl = CharIdxInLine(leftEnd)),
      Selection(right, LineIdx(0), from = CharIdxInLine(rightStart), toExcl = CharIdxInLine(rightEnd))
    )
  }

  "simple changes" in {
                  // 012345678901234567890123456789012
    val leftLine  = "One apple is going home tonight"
    val rightLine = "Two apples is coming home tonight"

    //println(NewAlignment.fromOld(Vector(leftLine), Vector(rightLine), alignment))
    //sortBy for easier comparison
    NewAlignment.fromOld(Vector(leftLine), Vector(rightLine), alignment).matches.toSeq.sortBy(_.left.from.i) shouldBe
      Set(
        m(leftLine, rightLine, leftStart = 4, leftEnd = 9, rightStart = 4, rightEnd = 10),
        m(leftLine, rightLine, leftStart = 10, leftEnd = 12, rightStart = 11, rightEnd = 13),
        m(leftLine, rightLine, leftStart = 19, leftEnd = 23, rightStart = 21, rightEnd = 25),
        m(leftLine, rightLine, leftStart = 24, leftEnd = 31, rightStart = 26, rightEnd = 33)
      ).toSeq.sortBy(_.left.from.i)
  }
}
