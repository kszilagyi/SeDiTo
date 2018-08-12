package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.LineIdx
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class LineRangeTest extends FreeSpecLike {
  private val range = LineRange(LineIdx(2), LineIdx(5))
  "without before is not changing" in {
    range.without(LineIdx(0)) shouldBe Seq(range)
  }

  "without after is not changing" in {
    range.without(LineIdx(10)) shouldBe Seq(range)
  }

  "without on left edge is not splitting" in {
    range.without(LineIdx(2)) shouldBe Seq(LineRange(LineIdx(3), LineIdx(5)))
  }

  "without on right edge is not splitting" in {
    range.without(LineIdx(4)) shouldBe Seq(LineRange(LineIdx(2), LineIdx(4)))
  }

  "without on middle is splitting" in {
    range.without(LineIdx(3)) shouldBe Seq(LineRange(LineIdx(2), LineIdx(3)), LineRange(LineIdx(4), LineIdx(5)))
  }

  "without can not result in an empty range" in {
    LineRange(LineIdx(2), LineIdx(3)).without(LineIdx(2)) shouldBe Seq()
  }

  "overlap with itself" in {
    range.overlap(range) shouldBe true
  }

  "overlap with a negative range" in {
    range.overlap(LineRange(range.to, range.from)) shouldBe false
  }

  "overlap on the left edge (negative case)" in {
    range.overlap(LineRange(LineIdx(0), LineIdx(2))) shouldBe false
  }

  "overlap on the right edge (negative case)" in {
    range.overlap(LineRange(LineIdx(5), LineIdx(10))) shouldBe false
  }

  "overlap on the left side " in {
    range.overlap(LineRange(LineIdx(0), LineIdx(3))) shouldBe true
  }

  "overlap on the right side " in {
    range.overlap(LineRange(LineIdx(4), LineIdx(10))) shouldBe true
  }

  "overlap inside " in {
    range.overlap(LineRange(LineIdx(3), LineIdx(4))) shouldBe true
  }

  "overlap outside " in {
    range.overlap(LineRange(LineIdx(0), LineIdx(10))) shouldBe true
  }

  "intersect works with two empty intervals" in {
    val orig = LineRange(LineIdx(0), LineIdx(0))
    orig.intersect(LineRange(LineIdx(1), LineIdx(1))) shouldBe None
  }

  "intersect works with two empty interval on left" in {
    val orig = LineRange(LineIdx(0), LineIdx(0))
    orig.intersect(LineRange(LineIdx(1), LineIdx(2))) shouldBe None
  }

  "intersect works with two empty interval on right" in {
    val right = LineRange(LineIdx(0), LineIdx(0))
    LineRange(LineIdx(1), LineIdx(2)).intersect(right) shouldBe None
  }

  "intersect unrelated non-empty ranges" in {
    range.intersect(LineRange(LineIdx(100), LineIdx(200))) shouldBe None
  }

  "intersect overlapping ranges" in {
    range.intersect(LineRange(LineIdx(4), LineIdx(6))) shouldBe Some(LineRange(LineIdx(4), LineIdx(5)))
  }

  "intersect with itself" in {
    range.intersect(range) shouldBe Some(range)
  }
}
