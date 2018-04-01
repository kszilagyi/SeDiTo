package com.kristofszilagyi.sedito.common

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
final case class CharIdxInLine(i: Int) {
  def +(other: Int): CharIdxInLine = CharIdxInLine(i + other)
  def +(other: CharIdxInLine): CharIdxInLine = CharIdxInLine(i + other.i)
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


object LineAlignment {
  implicit val format: JsonFormat[LineAlignment] = jsonFormat1(LineAlignment.apply)
}

//TODO error handling - there should be no no duplication of left or right
final case class LineAlignment(matches: Set[LineMatch]) {
  assert(matches.map(_.leftLineIdx).size ==== matches.size, s"$matches")
  assert(matches.map(_.rightLineIdx).size ==== matches.size, s"$matches")

  def partition: PartitionedAlignment = {
    val rightWithLeftOrdered = matches.toSeq.sortBy(_.leftLineIdx.i).map(_.rightLineIdx.i)
    //todo this assumes there is no duplication
    val longestRights = LongestIncreasingSubsequence.apply(rightWithLeftOrdered.toArray).asScala
    val notMoved = matches.filter(m => longestRights.contains(m.rightLineIdx.i))
    val moved = matches -- notMoved
    PartitionedAlignment(moved, notMoved)
  }

  def withMatch(m: LineMatch): LineAlignment = {
    LineAlignment(matches + m)
  }

  def conflict(m: LineMatch): Boolean = {
    matches.exists(_.conflict(m))
  }
}


final case class PartitionedAlignment(moved: Set[LineMatch], notMoved: Set[LineMatch])
