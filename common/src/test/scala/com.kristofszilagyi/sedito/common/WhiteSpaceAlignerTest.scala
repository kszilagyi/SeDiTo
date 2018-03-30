package com.kristofszilagyi.sedito.common

import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class WhiteSpaceAlignerTest extends FreeSpecLike {
  "empty" in {
    WhiteSpaceAligner.align(Lines.empty, Lines.empty, LineAlignment(Set.empty)) shouldBe LineAlignment(Set.empty)
  }

  "all aligned already" in {
    val left = ('a' to 'e').map(_.toString)
    val right = left
    val alignment = LineAlignment(left.indices.map(idx => LineMatch(LineIdx(idx), LineIdx(idx))).toSet)
    WhiteSpaceAligner.align(Lines(left), Lines(right), alignment) shouldBe alignment

  }

  "middle aligned, rest is not space" in {
    val left = ('a' to 'e').map(_.toString)
    val right = left
    val alignment = LineAlignment(Set(LineMatch(LineIdx(2), LineIdx(2))))
    WhiteSpaceAligner.align(Lines(left), Lines(right), alignment) shouldBe alignment
  }

  "middle aligned, rest is space" in {
    val left = IndexedSeq("", " ", "a", "\t", "   ")
    val right = IndexedSeq(" ", "  ", "a", "", "")
    val alignment = LineAlignment(Set(LineMatch(LineIdx(2), LineIdx(2))))
    val expected = LineAlignment(left.indices.map(idx => LineMatch(LineIdx(idx), LineIdx(idx))).toSet)
    WhiteSpaceAligner.align(Lines(left), Lines(right), alignment) shouldBe expected
  }
}
