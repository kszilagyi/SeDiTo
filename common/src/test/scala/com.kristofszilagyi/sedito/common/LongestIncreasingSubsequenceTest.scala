package com.kristofszilagyi.sedito.common

import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._
final class LongestIncreasingSubsequenceTest extends FreeSpecLike {
  "empty" in {
    LongestIncreasingSubsequence.apply(Array()) should contain theSameElementsInOrderAs List()
  }
  "trivial case works" in {
    LongestIncreasingSubsequence.apply(Array(1)) should contain theSameElementsInOrderAs List(1)
  }

  "case with 2 works" in {
    LongestIncreasingSubsequence.apply(Array(1, 2)) should contain theSameElementsInOrderAs List(1, 2)
  }

  "case with reversed 2 works" in {
    LongestIncreasingSubsequence.apply(Array(2, 1)) should contain theSameElementsInOrderAs List(1)
  }

  "case with 3 works (full)" in {
    LongestIncreasingSubsequence.apply(Array(1, 2, 3)) should contain theSameElementsInOrderAs List(1, 2, 3)
  }

  "case with 3 works (reverse)" in {
    LongestIncreasingSubsequence.apply(Array(3, 2, 1)) should contain theSameElementsInOrderAs List(1)
  }

  "case with 3 works (2 long sequence in the beginning)" in {
    LongestIncreasingSubsequence.apply(Array(1, 2, 0)) should contain theSameElementsInOrderAs List(1, 2)
  }

  "case with 3 works (2 long sequence in the end)" in {
    LongestIncreasingSubsequence.apply(Array(1, 0, 2)) should contain theSameElementsInOrderAs List(0, 2)
  }
}
