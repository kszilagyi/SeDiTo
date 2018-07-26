package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.Warts._
import com.kristofszilagyi.sedito.common.{LineIdx, LineMatch}

final case class LineRange(from: LineIdx, to: LineIdx) {
  def positive: Boolean = to.i - from.i > 0
}
object EquivalencePoint {
  def from(left: (Int, Int), right: (Int, Int)) =
    EquivalencePoint(LineRange(LineIdx(left._1), LineIdx(left._2)), LineRange(LineIdx(right._1), LineIdx(right._2)))
}

/**
  * If there is a zero length range that means that the insertion is just before the position
  *
  *
  */
final case class EquivalencePoint(left: LineRange, right: LineRange) {
  def positive: Boolean = left.positive || right.positive
}


object InsertionPointCalculator {
//  import InsertionPointCalculator.util._
//
//  private object util {
//    @tailrec
//    private def toRangesImpl(lines: List[LineIdx], finishedRanges: Seq[LineRange], rangeInProgress: LineRange): Seq[LineRange] = {
//      lines match {
//        case currentLine :: tail =>
//          if (rangeInProgress.to + 1 ==== currentLine) toRangesImpl(tail, finishedRanges, rangeInProgress.copy(to = currentLine))
//          else toRangesImpl(tail, finishedRanges :+ rangeInProgress, LineRange(currentLine, currentLine + 1))
//        case Nil =>
//          finishedRanges :+ rangeInProgress
//      }
//    }
//    // as it stands I am not sure we actually need this, it might be only for speedup...
//    def toRanges(lines: List[LineIdx]): Seq[LineRange] = {
//      lines match {
//        case firstLine :: tail => toRangesImpl(tail, Seq.empty, LineRange(firstLine, firstLine + 1))
//        case Nil => Seq.empty
//      }
//    }
//  }
//  private implicit class RichTraversableOnce[A](xs: TraversableOnce[A]) {
//    def maxByOption[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] = {
//      Try(xs.maxBy(f)).toOption
//    }
//  }

  def calc(notMoved: Traversable[LineMatch]): Traversable[EquivalencePoint] = {
//    notMoved.map { lineMatch =>
//      lineMatch.leftLineIdx
//
//    }
//    (1 to 10).end
//    // we have to find the first notMoved from the inserted in both directions. Then grab the other sides.
//    val insertedRanges = toRanges(inserted.toList)
//    insertedRanges.map { insertedRange =>
//      val from = insertedRange.from
//      val to = insertedRange.to
//      val nearestNotMovedBefore = notMoved.filter(_.rightLineIdx < from).maxByOption(_.rightLineIdx)
//      val nearestNotMovedAfter = notMoved.filter(_.rightLineIdx > to).maxByOption(_.rightLineIdx)
//      ne
//    }
    // sorting by left imply sorting by right
    val notMovedSorted = notMoved.toList.sortBy(_.leftLineIdx)
    @SuppressWarnings(Array(Var))
    var last = LineMatch.create(-1, -1)
    val builder = Seq.newBuilder[EquivalencePoint]
    // this could be done with sliding though I am not convinced it would be any better
    notMovedSorted.foreach { current =>
      val eq = EquivalencePoint(LineRange(last.leftLineIdx + 1, current.leftLineIdx), LineRange(last.rightLineIdx + 1, current.rightLineIdx))
      if (eq.positive)
        discard(builder += eq)
      last = current
    }
    builder.result()

  }
}
