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

}
