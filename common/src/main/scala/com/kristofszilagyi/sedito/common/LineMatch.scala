package com.kristofszilagyi.sedito.common

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
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

final case class LineIdx(i: Int) {
  def <(other: LineIdx): Boolean = i < other.i
  def +(other: Int): LineIdx = LineIdx(i + other)
}


@SuppressWarnings(Array(Warts.Overloading))
final case class CharIdxInLine(i: Int) extends Ordered[CharIdxInLine] {
  def +(other: Int): CharIdxInLine = CharIdxInLine(i + other)
  def +(other: CharIdxInLine): CharIdxInLine = CharIdxInLine(i + other.i)

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
  implicit val format: JsonFormat[UnambiguousLineAlignment] = jsonFormat1(UnambiguousLineAlignment.unsafeCreate)
  final case class AmbiguousMatch(duplicates: Set[LineMatch])

  @SuppressWarnings(Array(Warts.Throw))
  private def unsafeCreate(matches: Set[LineMatch]): UnambiguousLineAlignment = {
    create(matches) match {
      case Valid(a) => a
      case Invalid(e) => throw new RuntimeException(s"$e")
    }
  }
  def create(matches: Set[LineMatch]): ValidatedNel[AmbiguousMatch, UnambiguousLineAlignment] = {
    val leftDuplicates = matches.groupBy(_.leftLineIdx).filter(_._2.size > 1).values
    val rightDuplicates = matches.groupBy(_.rightLineIdx).filter(_._2.size > 1).values
    val maybeDuplicates = (leftDuplicates ++ rightDuplicates).toList.map(AmbiguousMatch)
    NonEmptyList.fromList(maybeDuplicates) match {
      case Some(duplicates) => Invalid(duplicates)
      case None => Valid(UnambiguousLineAlignment(matches))
    }
  }
}

sealed case class UnambiguousLineAlignment private(matches: Set[LineMatch]) {

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


final case class PartitionedAlignment(moved: Set[LineMatch], notMoved: Set[LineMatch])
