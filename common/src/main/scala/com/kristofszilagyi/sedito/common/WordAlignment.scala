package com.kristofszilagyi.sedito.common

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.kristofszilagyi.sedito.common.AmbiguousWordAlignment.resolveConflicts
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import spray.json.DefaultJsonProtocol._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.collection.Searching._

object Selection {
  /**
    * @param from         from within the line
    * @param absoluteFrom from within the whole string
    */
  def create(line: String, lineIdx: LineIdx, from: CharIdxInLine, toExcl: CharIdxInLine, absoluteFrom: Int): Validated[WordIndexRangeError, Selection] = {
    if (from.i < 0 || from.i >= line.length) Invalid(IndexIsOutOfRange(from.i, line))
    else if (toExcl.i < 0 || toExcl.i > line.length) Invalid(IndexIsOutOfRange(toExcl.i, line))
    else if (from.i >= toExcl.i) Invalid(RangeIsNotPositive(from.i, toExcl.i, line))
    else Valid(new Selection(line, lineIdx, from, toExcl, absoluteFrom) {})
  }

  //we are writing the line here, this is quite horrible...
  implicit val format: RootJsonFormat[Selection] =
    jsonFormat[String, LineIdx, CharIdxInLine, CharIdxInLine, Int, Selection](new Selection(_, _, _, _, _) {}, "line", "lineIdx", "from", "to", "absoluteFrom")

  implicit val ordering: Ordering[Selection] = Ordering.by[Selection, (LineIdx, CharIdxInLine)](s => (s.lineIdx, s.from))

  //this implementation is weird as it was only kept due to backward compatibility and was copied WordIndexRange
  def fromAbsolute(start: Int, end: Int, fullText: FullText): Validated[WordIndexRangeError, Selection] = {
    //todo line ending types
    val lineBreakIdxes = fullText.s.zipWithIndex.filter(_._1 ==== '\n').map(_._2).toArray.sorted
    val lineBreakBeforeInArray = math.abs(lineBreakIdxes.search(start).insertionPoint) - 1
    val lineBreakAfterInArray = lineBreakBeforeInArray + 1
    val lineBreakAfterInText =
      if (lineBreakAfterInArray < lineBreakIdxes.length) lineBreakIdxes(lineBreakAfterInArray)
      else fullText.s.length
    val lineIdx = LineIdx(lineBreakBeforeInArray + 1)
    val lineBreakBeforeInText =
      if (lineBreakBeforeInArray >= 0) lineBreakIdxes(lineBreakBeforeInArray)
      else -1
    val line = fullText.s.substring(lineBreakBeforeInText + 1, lineBreakAfterInText)
    val from = CharIdxInLine(start - (lineBreakBeforeInText + 1))
    val to = CharIdxInLine(end - (lineBreakBeforeInText + 1))
    Selection.create(line, lineIdx, from, to, start)
  }
}
//I am using this instead of indexing into the whole string so that line ending types do not make a difference
sealed abstract case class Selection private(line: String, lineIdx: LineIdx, from: CharIdxInLine, toExcl: CharIdxInLine, absoluteFrom: Int) {
  def toText: String = s"${line.substring(from.i, toExcl.i)}"

  override def toString: String = {
    s"${lineIdx.i}: ${from.i} - ${toExcl.i} [$toText]($absoluteFrom)"
  }

  def length: Int = toExcl.i - from.i

  def empty: Boolean = length <= 0
}

object WordMatch {
  implicit val jsonFormat: RootJsonFormat[WordMatch] = jsonFormat2(WordMatch.apply)

}
final case class WordMatch(left: Selection, right: Selection) {
  def readable: String = {
    s"${left.toText} - ${right.toText}"
  }
}

object AmbiguousWordAlignment {

  private final case class Ld(left: Selection, right: Selection, dist: Double)
  private final case class PossibleResult(result: Set[Ld])

  //TODO this now can fail on very long lines (though I think these are only called when reading test case?)
  @SuppressWarnings(Array(Warts.Recursion))
  private def approximatePossibleBestMatches(orderedLds: List[Ld], result: Set[Ld], branchingSoFar: Int): Set[PossibleResult] = {
    orderedLds match {
      case first :: rest =>
        val conflicts = rest.filter { r =>
          r.left ==== first.left || r.right ==== first.right
        }
        val conflictWithSameLd = conflicts.filter(_.dist ==== first.dist)
        val branchingNow = if (branchingSoFar < 1000) 4 else 1
        val potentials = (first +: conflictWithSameLd).take(branchingNow) // take - to limit processing time
        val possibleResults = potentials.flatMap { pot =>
          val withoutConflict = orderedLds.filterNot { r =>
            r.left ==== pot.left || r.right ==== pot.right
          }
          approximatePossibleBestMatches(withoutConflict, result + pot, branchingSoFar * potentials.size)
        }
        possibleResults.toSet
      case Nil => Set(PossibleResult(result))
    }
  }

  private def sortMatches(matches: Traversable[WordMatch]) = {
    val lineIdxOrdering = implicitly[Ordering[LineIdx]]
    val charOrdering = implicitly[Ordering[CharIdxInLine]]
    val ordering = Ordering.Tuple4(lineIdxOrdering, lineIdxOrdering, charOrdering, charOrdering)
    matches.toSeq.sortBy(m => (m.left.lineIdx, m.right.lineIdx, m.left.from, m.right.from))(ordering) //arbitrary but deterministic ordering
  }

  @SuppressWarnings(Array(Warts.TraversableOps))
  private def resolveConflicts(conflictMap: Map[(LineIdx, CharIdxInLine, CharIdxInLine), Traversable[WordMatch]]) = {
    conflictMap.map { case (_, conflictingMatches) =>
      sortMatches(conflictingMatches).head //this is safe because groupBy will never result in an empty list
    }
  }

  implicit val jsonFormat: RootJsonFormat[AmbiguousWordAlignment] = DefaultJsonProtocol.jsonFormat1(AmbiguousWordAlignment.apply)
}
/**
  * For explanation see the comment on AmbiguousLineAlignment
  */
final case class AmbiguousWordAlignment(matches: Set[WordMatch]) {
  def readable: String = matches.map(_.readable).mkString(", ")

  def toUnambigous: UnambiguousWordAlignment = {
    //we assume no overlap. So every conflict is caused by a word being matched with multiple other words
    val leftMap = matches.groupBy(m => (m.left.lineIdx, m.left.from, m.left.toExcl))
    val leftResolved = resolveConflicts(leftMap)
    val rightMap = leftResolved.groupBy(m => (m.right.lineIdx, m.right.from, m.right.toExcl))
    val botResolved = resolveConflicts(rightMap)
    UnambiguousWordAlignment(botResolved.toSet)
  }
}

//I am undecided if this should check for conflicts or not. Same for UnambiguousLineAlignment
final case class UnambiguousWordAlignment(matches: Set[WordMatch]) {
  def readable: String = matches.map(_.readable).mkString(", ")
}