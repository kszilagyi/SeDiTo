package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.{LineIdx, LineMatch}
import com.kristofszilagyi.sedito.gui.InsertionPointCalculator.calc
import com.kristofszilagyi.sedito.gui.InsertionPointCalculatorTest.pairs
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

object InsertionPointCalculatorTest {
  //private def lines(seq: Seq[Int]) = seq.map(LineIdx.apply).toIndexedSeq
  private def pairs(matches: Map[Int, Int]) = matches.map(m => LineMatch(LineIdx(m._1), LineIdx(m._2)))
}
final class InsertionPointCalculatorTest extends FreeSpecLike {
  "empty" in {
    calc(notMoved = IndexedSeq.empty, moved = Seq.empty) shouldBe Traversable.empty
  }

  "no insert or delete" in {
    calc(notMoved = pairs((0 to 10).map(i => i -> i).toMap), moved = Seq.empty) shouldBe Traversable.empty
  }

  "simple insert" in {
    calc(notMoved = pairs(Map(0 -> 0, 1 -> 3)), moved = Seq.empty) shouldBe Traversable(EquivalencePoint.from((1, 1), (1, 3)))
  }

  "simple delete" in {
    calc(notMoved = pairs(Map(0 -> 0, 3 -> 1)), moved = Seq.empty) shouldBe Traversable(EquivalencePoint.from((1, 3), (1, 1)))
  }

  "insert + delete" in {
    calc(notMoved = pairs(Map(0 -> 0, 3 -> 3)), moved = Seq.empty) shouldBe Traversable(EquivalencePoint.from((1, 3), (1, 3)))
  }

  "insert + move" in {
    calc(notMoved = pairs(Map(0 -> 0, 1 -> 3)), moved = pairs(Map(20 -> 2))) shouldBe Traversable(EquivalencePoint.from((1, 1), (1, 2)))
  }

  "remove + move" in {
    calc(notMoved = pairs(Map(0 -> 0, 3 -> 1)), moved = pairs(Map(2 -> 20))) shouldBe Traversable(EquivalencePoint.from((1, 2), (1, 1)))
  }
  //test move splits
}
