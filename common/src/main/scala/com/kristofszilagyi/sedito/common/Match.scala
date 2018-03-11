package com.kristofszilagyi.sedito.common

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
  def +(other: Int): LineIdx = LineIdx(i + 1)
}


final case class CharIdxInLine(i: Int) {
  def +(other: Int): CharIdxInLine = CharIdxInLine(i + other)
}

object Match {
  implicit val format: JsonFormat[Match] = jsonFormat2(Match.apply)

  def create(leftLineIdx: Int, rightLineIdx: Int): Match = new Match(LineIdx(leftLineIdx), LineIdx(rightLineIdx))
}
final case class Match(leftLineIdx: LineIdx, rightLineIdx: LineIdx)


object Alignment {
  implicit val format: JsonFormat[Alignment] = jsonFormat1(Alignment.apply)
}

//TODO error handling - there should be no no duplication of left or right
final case class Alignment(matches: Set[Match]) {
  def partition: PartitionedAlignment = {
    val rightWithLeftOrdered = matches.toSeq.sortBy(_.leftLineIdx.i).map(_.rightLineIdx.i)
    //todo this assumes there is no duplication
    val longestRights = LongestIncreasingSubsequence.apply(rightWithLeftOrdered.toArray).asScala
    val notMoved = matches.filter(m => longestRights.contains(m.rightLineIdx.i))
    val moved = matches -- notMoved
    PartitionedAlignment(moved, notMoved)
  }
}


final case class PartitionedAlignment(moved: Set[Match], notMoved: Set[Match])
