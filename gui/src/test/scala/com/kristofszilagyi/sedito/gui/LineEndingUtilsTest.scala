package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.FullText
import com.kristofszilagyi.sedito.gui.utils.LineEndingUtils
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class LineEndingUtilsTest extends FreeSpecLike {
  "find windows line ending" in {
    LineEndingUtils.guessLineEnding(FullText("a\r\nb")) shouldBe "\r\n"
  }

  "find window line ending" in {
    LineEndingUtils.guessLineEnding(FullText("a\nb")) shouldBe "\n"
  }

  "find mac line ending" in {
    LineEndingUtils.guessLineEnding(FullText("a\rb")) shouldBe "\r"
  }
}
