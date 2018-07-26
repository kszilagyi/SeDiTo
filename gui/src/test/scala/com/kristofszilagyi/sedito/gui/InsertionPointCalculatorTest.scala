package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.{LineIdx, LineMatch}
import com.kristofszilagyi.sedito.gui.InsertionPointCalculatorTest.pairs
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

object InsertionPointCalculatorTest {
  //private def lines(seq: Seq[Int]) = seq.map(LineIdx.apply).toIndexedSeq
  private def pairs(matches: Map[Int, Int]) = matches.map(m => LineMatch(LineIdx(m._1), LineIdx(m._2)))
}
final class InsertionPointCalculatorTest extends FreeSpecLike {
  "empty" in {
    InsertionPointCalculator.calc(notMoved = IndexedSeq.empty) shouldBe Traversable.empty
  }

  "no insert or delete" in {
    InsertionPointCalculator.calc(notMoved = pairs((0 to 10).map(i => i -> i).toMap)) shouldBe Traversable.empty
  }

  "simple insert" in {
    InsertionPointCalculator.calc(notMoved = pairs(Map(0 -> 0, 1 -> 3))) shouldBe Traversable(EquivalencePoint.from((1, 1), (1, 3)))
  }

  "simple delete" in {
    InsertionPointCalculator.calc(notMoved = pairs(Map(0 -> 0, 3 -> 1))) shouldBe Traversable(EquivalencePoint.from((1, 3), (1, 1)))
  }

  "insert + delete" in {
    InsertionPointCalculator.calc(notMoved = pairs(Map(0 -> 0, 3 -> 3))) shouldBe Traversable(EquivalencePoint.from((1, 3), (1, 3)))
  }
}
