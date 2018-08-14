package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.LineIdx
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class DiffPaneTest extends FreeSpecLike {
  "offScreenY no-op if on and off is the same" in {
    DiffPane.offScreenY(on = LineIdx(1), off = LineIdx(1), height = 25, onY = 125) shouldBe 125
  }

  "on is bigger" in {
    DiffPane.offScreenY(on = LineIdx(2), off = LineIdx(1), height = 25, onY = 125) shouldBe 100
  }

  "off is bigger" in {
    DiffPane.offScreenY(on = LineIdx(1), off = LineIdx(2), height = 25, onY = 125) shouldBe 150
  }
}
