package com.kristofszilagyi.sedito.common

import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class LineAlignerTest extends FreeSpecLike {
  "empty" in {
    LineAligner.align(UnambiguousWordAlignment(Set.empty)) shouldBe UnambiguousLineAlignment(Set.empty)
  }

  private def selection(line: String, lineIdx: Int, from: Int, to: Int) = {
    Selection.create(line, LineIdx(lineIdx), CharIdxInLine(from), CharIdxInLine(to)).getOrElse(fail("wrong test data"))
  }
  "one" in {
    LineAligner.align(UnambiguousWordAlignment(Set(WordMatch(
      selection("line0", 0, 0, 5),
      selection("line0", 0, 0, 5)
    )))) shouldBe UnambiguousLineAlignment(Set(LineMatch(LineIdx(0), LineIdx(0))))
  }

  /**
    * line0 { -> line0
    *            {
    */
  "slip line into two" in {
    LineAligner.align(UnambiguousWordAlignment(Set(
      WordMatch(
        selection("line0 {", lineIdx = 0, from = 0, to = 5),
        selection("line0", lineIdx = 0, from = 0, to = 5)
      ),
      WordMatch(
        selection("line0 {", lineIdx = 0, from = 6, to = 7),
        selection("{", lineIdx = 1, from = 0, to = 1)
      )
    ))) shouldBe UnambiguousLineAlignment(Set(LineMatch(LineIdx(0), LineIdx(0))))
  }

  /**
    * line0 -> line0 {
    * {
    */
  "merge lines" in {
    LineAligner.align(UnambiguousWordAlignment(Set(
      WordMatch(
        selection("line0", lineIdx = 0, from = 0, to = 5),
        selection("line0 {", lineIdx = 0, from = 0, to = 5)
      ),
      WordMatch(
        selection("{", lineIdx = 1, from = 0, to = 1),
        selection("line0 {", lineIdx = 0, from = 6, to = 7)
      )
    ))) shouldBe UnambiguousLineAlignment(Set(LineMatch(LineIdx(0), LineIdx(0))))
  }

  /**
    * line0 line1 -> line0
    *                line1
    */
  "ambigous alignment results in 1 result" in {
    LineAligner.align(UnambiguousWordAlignment(Set(
      WordMatch(
        selection("line0 line1", lineIdx = 0, from = 0, to = 5),
        selection("line0", lineIdx = 0, from = 0, to = 5)
      ),
      WordMatch(
        selection("line0 line1", lineIdx = 0, from = 6, to = 11),
        selection("line1", lineIdx = 1, from = 0, to = 5)
      )
    ))).matches should have size 1
  }

  /**
    * map a b -> map
    *            a b
    */
  "a 3 letter word is stronger than 2 1 letter words" in {
    LineAligner.align(UnambiguousWordAlignment(Set(
      WordMatch(
        selection("map a b", lineIdx = 0, from = 0, to = 3),
        selection("map", lineIdx = 0, from = 0, to = 3)
      ),
      WordMatch(
        selection("map a b", lineIdx = 0, from = 4, to = 5),
        selection("a b", lineIdx = 1, from = 0, to = 1)
      ),
      WordMatch(
        selection("map a b", lineIdx = 0, from = 6, to = 7),
        selection("a b", lineIdx = 1, from = 2, to = 3)
      )
    ))) shouldBe UnambiguousLineAlignment(Set(LineMatch(LineIdx(0), LineIdx(0))))
  }
}
