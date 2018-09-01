package com.kristofszilagyi.sedito.common

import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._
import WordAlignmentToUnambiguousTest._
import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated

object WordAlignmentToUnambiguousTest {
  private def selection(s: String, idx: Int) = {
    Selection.create(s, LineIdx(idx), CharIdxInLine(0), CharIdxInLine(s.length), -1).getAssert("")
  }
  private val l0 = selection("left zero", 0)
  private val l1 = selection("left one", 1)
  private val l2 = selection("left two", 2)

  private val r0 = selection("right zero", 0)
  private val r1 = selection("right one", 1)
  private val r2 = selection("right two", 2)
}

final class WordAlignmentToUnambiguousTest extends FreeSpecLike{
  "empty" in {
    AmbiguousWordAlignment(Set.empty).toUnambigous shouldBe UnambiguousWordAlignment(Set.empty)
  }

  "no conflict - 1" in {
    val matches = Set(WordMatch(l0, r0))
    AmbiguousWordAlignment(matches).toUnambigous shouldBe UnambiguousWordAlignment(matches)
  }

  "no conflict - 2" in {
    val matches = Set(WordMatch(l0, r0), WordMatch(l1, r1))
    AmbiguousWordAlignment(matches).toUnambigous shouldBe UnambiguousWordAlignment(matches)
  }

  "1 conflict left" in {
    AmbiguousWordAlignment(Set(WordMatch(l0, r0), WordMatch(l0, r1))).toUnambigous shouldBe
      UnambiguousWordAlignment(Set(WordMatch(l0, r0)))
  }

  "1 conflict right" in {
    AmbiguousWordAlignment(Set(WordMatch(l0, r0), WordMatch(l1, r0))).toUnambigous shouldBe
      UnambiguousWordAlignment(Set(WordMatch(l0, r0)))
  }

  "1 conflict left, 1 conflict right, independent" in {
    AmbiguousWordAlignment(Set(WordMatch(l0, r0), WordMatch(l0, r1), WordMatch(l1, r2), WordMatch(l2, r2))).toUnambigous shouldBe
      UnambiguousWordAlignment(Set(WordMatch(l0, r0), WordMatch(l1, r2)))
  }

  "1 conflict left, 1 conflict right, chained" in {
    AmbiguousWordAlignment(Set(WordMatch(l0, r0), WordMatch(l0, r1), WordMatch(l1, r1))).toUnambigous shouldBe
      UnambiguousWordAlignment(Set(WordMatch(l0, r0), WordMatch(l1, r1)))
  }
}
