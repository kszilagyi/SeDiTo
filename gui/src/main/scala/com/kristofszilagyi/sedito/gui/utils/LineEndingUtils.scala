package com.kristofszilagyi.sedito.gui.utils

import com.kristofszilagyi.sedito.common.FullText

object LineEndingUtils {
  def guessLineEnding(s: FullText): String = {
    "(\n|\r\n|\r)".r.findFirstIn(s.s).getOrElse(System.lineSeparator())
  }
}
