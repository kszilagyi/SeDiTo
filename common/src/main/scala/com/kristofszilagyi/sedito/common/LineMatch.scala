package com.kristofszilagyi.sedito.common

import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.utils.JsonUtils._
import spray.json.DefaultJsonProtocol._
import spray.json.{JsNumber, JsValue, JsonFormat}

import scala.collection.JavaConverters._

object LineIdx {
  implicit val format: JsonFormat[LineIdx] = new JsonFormat[LineIdx] {
    def write(idx: LineIdx): JsValue = JsNumber(idx.i)

    def read(json: JsValue): LineIdx = json match {
      case JsNumber(value) => LineIdx(value.toIntExact)
      case other => spray.json.deserializationError(s"Expected JsonNumber got $other instead")
    }
  }
}

final case class LineIdx(i: Int) extends AnyVal with Ordered[LineIdx]  {
  def +(other: Int): LineIdx = LineIdx(i + other)

  def compare(that: LineIdx): Int = i.compareTo(that.i)
}

object CharIdxInLine {
  implicit val format: JsonFormat[CharIdxInLine] = wrappedInt(CharIdxInLine.apply)(_.i)
}
@SuppressWarnings(Array(Warts.Overloading))
final case class CharIdxInLine(i: Int) extends AnyVal with Ordered[CharIdxInLine] {
  def +(other: Int): CharIdxInLine = CharIdxInLine(i + other)

  def compare(that: CharIdxInLine): Int = {
    i.compare(that.i)
  }
}

object LineMatch {
  implicit val format: JsonFormat[LineMatch] = jsonFormat2(LineMatch.apply)

  def create(leftLineIdx: Int, rightLineIdx: Int): LineMatch = new LineMatch(LineIdx(leftLineIdx), LineIdx(rightLineIdx))
}
final case class LineMatch(leftLineIdx: LineIdx, rightLineIdx: LineIdx) {
  def conflict(other: LineMatch): Boolean = {
    leftLineIdx ==== other.leftLineIdx || rightLineIdx ==== other.rightLineIdx
  }
}


object UnambiguousLineAlignment {
//  final case class AmbiguousMatch(duplicates: Set[LineMatch])

//  @SuppressWarnings(Array(Warts.Throw))
//  private def unsafeCreate(matches: Set[LineMatch]): UnambiguousLineAlignment = {
//    create(matches) match {
//      case Valid(a) => a
//      case Invalid(e) => throw new RuntimeException(s"$e")
//    }
//  }
//  def create(matches: Set[LineMatch]): ValidatedNel[AmbiguousMatch, UnambiguousLineAlignment] = {
//    val leftDuplicates = matches.groupBy(_.leftLineIdx).filter(_._2.size > 1).values
//    val rightDuplicates = matches.groupBy(_.rightLineIdx).filter(_._2.size > 1).values
//    val maybeDuplicates = (leftDuplicates ++ rightDuplicates).toList.map(AmbiguousMatch)
//    NonEmptyList.fromList(maybeDuplicates) match {
//      case Some(duplicates) => Invalid(duplicates)
//      case None => Valid(new UnambiguousLineAlignment(matches) {})
//    }
//  }
}

final case class UnambiguousLineAlignment private(matches: Set[LineMatch]) {

  def partition: PartitionedAlignment = {
    val rightWithLeftOrdered = matches.toSeq.sortBy(_.leftLineIdx.i).map(_.rightLineIdx.i)
    //todo this assumes there is no duplication
    val longestRights = LongestIncreasingSubsequence.apply(rightWithLeftOrdered.toArray).asScala
    val notMoved = matches.filter(m => longestRights.contains(m.rightLineIdx.i))
    val moved = matches -- notMoved
    PartitionedAlignment(moved, notMoved)
  }

  def withMatch(m: LineMatch): UnambiguousLineAlignment = {
    UnambiguousLineAlignment(matches + m)
  }

  def conflict(m: LineMatch): Boolean = {
    matches.exists(_.conflict(m))
  }
}

object AmbiguousLineAlignment {
  implicit val format: JsonFormat[AmbiguousLineAlignment] = jsonFormat1(AmbiguousLineAlignment.apply)
}
/**
  * It's important to be able to represent this as some matches are truly ambiguous: even a human
  * cannot say if any of them is better. Test cases can have alignments like. We must train the AI
  * to recognise both of those matches. We cannot display anything like this so a post-processing has to be made
  * which somehow selects one of the match. Which one to select is not important.
  */
final case class AmbiguousLineAlignment (matches: Set[LineMatch])

final case class PartitionedAlignment(moved: Set[LineMatch], notMoved: Set[LineMatch])
