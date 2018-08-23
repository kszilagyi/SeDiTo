package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.LineIdx
import com.kristofszilagyi.sedito.gui.Scroller.{Aligned, LeftIsLower, NothingOnScreen, RightIsLower}
import com.kristofszilagyi.sedito.gui.ScrollerTest.pair
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

import scala.collection.immutable.TreeMap
object ScrollerTest {
  def pair(i: Int): (LineIdx, LineIdx) = {
    (LineIdx(i), LineIdx(i))
  }
}

final class ScrollerTest extends FreeSpecLike {
  "empty" in {
    Scroller.calc(LineRange(LineIdx(0), LineIdx(0)), LineRange(LineIdx(0), LineIdx(0)),
      TreeMap.empty) shouldBe NothingOnScreen
  }


  "no alignment" in {
    Scroller.calc(LineRange(LineIdx(0), LineIdx(10)), LineRange(LineIdx(0), LineIdx(20)),
      TreeMap.empty) shouldBe Aligned
  }

  "vanilla, left is lower, middle is matched" in {
    Scroller.calc(LineRange(LineIdx(0), LineIdx(50)), LineRange(LineIdx(50), LineIdx(100)),
      TreeMap((1 to 100).map(i => pair(i)): _*)) shouldBe LeftIsLower
  }

  "vanilla, right is lower, middle is matched" in {
    Scroller.calc(LineRange(LineIdx(50), LineIdx(100)), LineRange(LineIdx(0), LineIdx(50)),
      TreeMap((1 to 100).map(i => pair(i)): _*)) shouldBe RightIsLower
  }

  "vanilla, left is lower, middle is unmatched" in {
    Scroller.calc(LineRange(LineIdx(0), LineIdx(50)), LineRange(LineIdx(50), LineIdx(100)),
      TreeMap((1 to 3).map(i => pair(i)): _*)) shouldBe LeftIsLower
  }

  "vanilla, right is lower, middle is unmatched" in {
    Scroller.calc(LineRange(LineIdx(50), LineIdx(100)), LineRange(LineIdx(0), LineIdx(50)),
      TreeMap((1 to 3).map(i => pair(i)): _*)) shouldBe RightIsLower
  }
}
