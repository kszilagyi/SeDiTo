package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.Warts.discard
import info.debatty.java.stringsimilarity.Levenshtein
import org.apache.commons.text.similarity.LongestCommonSubsequence
import org.scalatest.FreeSpecLike

import scala.util.Random

class Measurements extends FreeSpecLike {
  def measureSpeed(d: (String, String) => Any) = {
    val r = new Random(0)
    val s: Traversable[String] = (10000 to 10004) map { len =>
      ((1 to len) map { _ => r.nextPrintableChar() }).mkString
    }
    println(s"amount of string: ${s.size}")
    s foreach { s1 =>
      s foreach { s2 =>
        discard(d(s1, s2))
      }
    }
  }

  "lcs speed" in {
    val lcs = new LongestCommonSubsequence()
    measureSpeed(lcs.longestCommonSubsequence)
  }

  "lev speed" in {
    val lev = new Levenshtein()
    measureSpeed(lev.distance)
  }
}
