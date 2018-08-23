package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.{LineIdx, LineMatch}
import com.kristofszilagyi.sedito.gui.Scroller.{Aligned, LeftIsLower, NothingOnScreen, RightIsLower}
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class ScrollerTest extends FreeSpecLike {
  "empty" in {
    Scroller.calc(LineRange(LineIdx(0), LineIdx(0)), LineRange(LineIdx(0), LineIdx(0)),
      Set.empty) shouldBe NothingOnScreen
  }


  "no alignment" in {
    Scroller.calc(LineRange(LineIdx(0), LineIdx(10)), LineRange(LineIdx(0), LineIdx(20)),
      Set.empty) shouldBe Aligned
  }

  "vanilla, left is lower, middle is matched" in {
    Scroller.calc(LineRange(LineIdx(0), LineIdx(50)), LineRange(LineIdx(50), LineIdx(100)),
      (1 to 100).map(i => LineMatch.create(i, i)).toSet) shouldBe LeftIsLower
  }

  "vanilla, right is lower, middle is matched" in {
    Scroller.calc(LineRange(LineIdx(50), LineIdx(100)), LineRange(LineIdx(0), LineIdx(50)),
      (1 to 100).map(i => LineMatch.create(i, i)).toSet) shouldBe RightIsLower
  }
}
