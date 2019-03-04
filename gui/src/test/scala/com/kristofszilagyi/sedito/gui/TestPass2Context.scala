package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import com.kristofszilagyi.sedito.common.{FullText, Selection, UnambiguousWordAlignment, WordMatch}
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class TestPass2Context extends FreeSpecLike {
  private val fullText = FullText("0123456789" * 10)
  private def fixedLengthSelection(from: Int) = Selection.fromAbsolute(from, from + 5, fullText).getAssert
  private def m(left: Int, right: Int) = WordMatch(fixedLengthSelection(left), fixedLengthSelection(right))(Some(1))
  private val alignment = UnambiguousWordAlignment(Set(
    m(0, 0),
    m(5, 5),
    m(10, 10),
    m(20, 90)
  ))


  private val leftSortedMatches = alignment.matches.toVector.sortBy(_.left.absoluteFrom)

  "symmetric 1" in {
    TrainPass2.context(m(5, 5), leftSortedMatches, 5) shouldBe
      Traversable(m(0, 0), m(10, 10))
  }

  "100" in {
    TrainPass2.context(m(5, 5), leftSortedMatches, 100) shouldBe
      Vector(m(0, 0),
        m(10, 10),
        m(20, 90)
      )
  }
}
