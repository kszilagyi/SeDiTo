package com.kristofszilagyi.sedito.aligner

import org.scalatest.{FreeSpecLike}
import org.scalatest.Matchers._
import LongestCommonSubstringTest._
import com.kristofszilagyi.sedito.common.Warts.discard
object LongestCommonSubstringTest {
  def test(left: String, right: String, expected: String): Unit = {
    val res = LongestCommonSubstring.apply(left, right)
    val leftRes = left.substring(res.leftStart, res.leftStart + res.length)
    val rightRes = right.substring(res.rightStart, res.rightStart + res.length)
    leftRes shouldBe expected
    discard(rightRes shouldBe expected)
  }
}
class LongestCommonSubstringTest extends FreeSpecLike {
  "empty" in {
    test("", "", "")
  }

  "emptyLeft" in {
    test("", "adada", "")
  }

  "emptyRight" in {
    test("adadad", "", "")
  }

  "in the end" in {
    test("No Common Substring", "There is Common Substring", " Common Substring")
  }

  "in the end reverse" in {
    test("There is Common Substring", "No Common Substring", " Common Substring")
  }

  "in the middle" in {
    test("No Common Substring in the end", "There is Common Substring ever", " Common Substring ")
  }

  "in the middle reverse" in {
    test("There is Common Substring ever", "No Common Substring in the end", " Common Substring ")
  }

  "in the beginning" in {
    test("Common Substring there is", "Common Substring none there is", "Common Substring ")
  }

  "in the beginning reverse" in {
    test("Common Substring none there is", "Common Substring there is", "Common Substring ")
  }

  "two strings are the same" in {
    test("Common Substring ", "Common Substring ", "Common Substring ")
  }

  "first substring of the second" in {
    test("Common Substring ", "There is no Common Substring here", "Common Substring ")
  }

  "second substring of the first" in {
    test("There is no Common Substring here", "Common Substring ", "Common Substring ")
  }
}
