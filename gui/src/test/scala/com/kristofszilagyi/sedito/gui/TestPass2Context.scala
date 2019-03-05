package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import com.kristofszilagyi.sedito.common.{FullText, Selection, UnambiguousWordAlignment, WordMatch}
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class TestPass2Context extends FreeSpecLike {
  private val fullText = FullText("0123456789" * 30)
  private def fixedLengthSelection(from: Int) = Selection.fromAbsolute(from, from + 5, fullText).getAssert
  private def m(left: Int, right: Int) = WordMatch(fixedLengthSelection(left), fixedLengthSelection(right))(Some(1))
  private val alignment = UnambiguousWordAlignment(Set(
    m(0, 0),
    m(5, 5),
    m(10, 10),
    m(20, 90),
    m(200, 200),
    m(290, 220)
  ))


  private val leftSortedMatches = alignment.matches.toVector.sortBy(_.left.absoluteFrom)

  "0" in {
    TrainPass2.context(m(5, 5), leftSortedMatches, contextSize = 0) shouldBe Traversable.empty
  }
  "symmetric 1" in {
    TrainPass2.context(m(5, 5), leftSortedMatches, contextSize = 1) shouldBe Traversable(m(0, 0), m(10, 10))
  }

  "100" in {
    TrainPass2.context(m(5, 5), leftSortedMatches, contextSize = 100) shouldBe Vector(m(0, 0), m(10, 10), m(20, 90))
  }

  "half out" in {
    TrainPass2.context(m(5, 5), leftSortedMatches, contextSize = 30) shouldBe Vector(m(0, 0), m(10, 10))
  }

  "right before border - out" in {
    TrainPass2.context(m(20, 90), leftSortedMatches, contextSize = 75) shouldBe Traversable.empty
  }

  "right before border - in" in {
    TrainPass2.context(m(20, 90), leftSortedMatches, contextSize = 76) shouldBe Vector(m(10, 10))
  }

  "right after border - out" in {
    TrainPass2.context(m(10, 10), leftSortedMatches, contextSize = 75) shouldBe Vector(m(5, 5), m(0, 0))
  }

  "right after border - in" in {
    TrainPass2.context(m(10, 10), leftSortedMatches, contextSize = 76) shouldBe Vector(m(5, 5),m(0, 0), m(20, 90))
  }

  "left border before - out" in {
    TrainPass2.context(m(290, 220), leftSortedMatches, contextSize = 85) shouldBe Traversable.empty
  }

  "left border before - in" in {
    TrainPass2.context(m(290, 220), leftSortedMatches, contextSize = 86) shouldBe Vector(m(200, 200))
  }

  "left after before - out" in {
    TrainPass2.context(m(200, 200), leftSortedMatches, contextSize = 85) shouldBe Traversable.empty
  }

  "left after before - in" in {
    TrainPass2.context(m(200, 200), leftSortedMatches, contextSize = 86) shouldBe Vector(m(290, 220))
  }

  "non-existent center 0" in {
    TrainPass2.context(m(15,  15), leftSortedMatches, contextSize = 0) shouldBe Traversable.empty
  }

  "non-existent center 1" in {
    TrainPass2.context(m(15,  15), leftSortedMatches, contextSize = 1) shouldBe Vector(m(10, 10))
  }

  "non-existent center - out" in {
    TrainPass2.context(m(295,  295), leftSortedMatches, contextSize = 10) shouldBe Traversable.empty
  }
}
