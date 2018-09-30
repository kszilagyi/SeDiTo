package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import com.kristofszilagyi.sedito.common.{FullText, Selection, UnambiguousWordAlignment, WordMatch}
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

class TrivialContextCollectorTest extends FreeSpecLike {
  "empty" in {
    TrivialContextCorrector.correct(FullText(""), FullText(""), UnambiguousWordAlignment(Set.empty)) shouldBe
      UnambiguousWordAlignment(Set.empty)
  }

  "empty alignment" in {
    TrivialContextCorrector.correct(FullText("something here"), FullText("nothing there"), UnambiguousWordAlignment(Set.empty)) shouldBe
      UnambiguousWordAlignment(Set.empty)
  }

  "forward edge" in {
    val left = FullText("<include,")
    val right = FullText("<include;")
    val m1 = WordMatch(
      Selection.fromAbsolute(1, 8, left).getAssert,
      Selection.fromAbsolute(1, 8, right).getAssert
    )
    val m2 = WordMatch(
      Selection.fromAbsolute(0, 1, left).getAssert,
      Selection.fromAbsolute(0, 1, right).getAssert
    )
    TrivialContextCorrector.correct(left, right, UnambiguousWordAlignment(Set.empty)) shouldBe
      UnambiguousWordAlignment(Set(m1, m2))
  }

  "forward middle" in {
    val left = FullText(".com.sun")
    val right = FullText(",com.oracle")
    val m1 = WordMatch(
      Selection.fromAbsolute(1, 4, left).getAssert,
      Selection.fromAbsolute(1, 4, right).getAssert
    )
    val m2 = WordMatch(
      Selection.fromAbsolute(4, 5, left).getAssert,
      Selection.fromAbsolute(4, 5, right).getAssert
    )
    TrivialContextCorrector.correct(left, right, UnambiguousWordAlignment(Set(m1))) shouldBe
      UnambiguousWordAlignment(Set(m1, m2))
  }

  "backward edge" in {
    val left = FullText(",include>")
    val right = FullText("/include>")
    val m1 = WordMatch(
      Selection.fromAbsolute(1, 8, left).getAssert,
      Selection.fromAbsolute(1, 8, right).getAssert
    )
    val m2 = WordMatch(
      Selection.fromAbsolute(8, 9, left).getAssert,
      Selection.fromAbsolute(8, 9, right).getAssert
    )
    TrivialContextCorrector.correct(left, right, UnambiguousWordAlignment(Set.empty)) shouldBe
      UnambiguousWordAlignment(Set(m1, m2))
  }

  "backward middle" in {
    val left = FullText(".com.sun")
    val right = FullText(",com.oracle")
    val m1 = WordMatch(
      Selection.fromAbsolute(4, 5, left).getAssert,
      Selection.fromAbsolute(4, 5, right).getAssert
    )
    val m2 = WordMatch(
      Selection.fromAbsolute(1, 4, left).getAssert,
      Selection.fromAbsolute(1, 4, right).getAssert
    )
    TrivialContextCorrector.correct(left, right, UnambiguousWordAlignment(Set(m1))) shouldBe
      UnambiguousWordAlignment(Set(m1, m2))
  }

  //todo test for conflicting changes
  //todo test for self-conflicting changes
  //spaces?
}
