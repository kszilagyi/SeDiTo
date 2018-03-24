package com.kristofszilagyi.sedito.common

import com.kristofszilagyi.sedito.common.AssertionEx.fail
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import info.debatty.java.stringsimilarity.Levenshtein
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

//I am using this instead of indexing into the whole string so that line ending types do not make a difference
final case class Selection(line: String, lineIdx: LineIdx, from: CharIdxInLine, toExcl: CharIdxInLine) {
  def readable: String = s"${line.substring(from.i, toExcl.i)}"

  override def toString: String = {
    s"${lineIdx.i}: ${from.i} - ${toExcl.i} [$readable]"
  }
}
final case class NewMatch(left: Selection, right: Selection) {
  def readble: String = {
    s"${left.readable} - ${right.readable}"
  }
}

object NewAlignment {
  def fromOld(left: IndexedSeq[String], right: IndexedSeq[String], alignment: Alignment): NewAlignment = {
    val allMatches = alignment.matches.flatMap { m =>
      val leftLine = left(m.leftLineIdx.i)
      val rightLine = right(m.rightLineIdx.i)
      val leftWordRanges = Wordizer.toWordIndices(leftLine)
      val rightWordRanges = Wordizer.toWordIndices(rightLine)
      val ldCalculator = new Levenshtein()
      val lds = leftWordRanges.flatMap { leftRange =>
        rightWordRanges.map { rightRange =>
          val leftWord = leftRange.toWord
          val rightWord = rightRange.toWord
          (leftRange, rightRange) -> ldCalculator.distance(leftWord, rightWord)
        }
      }

      val leftLookup = lds.toList.groupBy(_._1._1)
      val matches = leftWordRanges.flatMap { leftRange =>
        val matches = leftLookup.getOrElse(leftRange, fail(s"Bug in code: $leftRange wasn't found in lookup table. " +
          s"leftLine: $leftLine, rightLine: $rightLine"))
        val bestMatch = matches.filter { case ((l, r), ld) =>
          ld <= (l.toWord.length + r.toWord.length) / 2 / 3
        }.sortBy(_._2).lastOption
        bestMatch.map(_._1).toList
      }
      val newMatchesForLine = matches.map { case (l, r) =>
        NewMatch(
          Selection(leftLine, m.leftLineIdx, CharIdxInLine(l.startIncl), CharIdxInLine(l.endExcl)),
          Selection(rightLine, m.rightLineIdx, CharIdxInLine(r.startIncl), CharIdxInLine(r.endExcl))
        )
      }
      newMatchesForLine
    }
    NewAlignment(allMatches)
  }
}
final case class NewAlignment(matches: Set[NewMatch]) {
  def readble: String = matches.map { m =>
    m.readble
  }.mkString(", ")
}

object Alignment {
  implicit val format: JsonFormat[Alignment] = jsonFormat1(Alignment.apply)
}

//TODO error handling - there should be no no duplication of left or right
final case class Alignment(matches: Set[Match]) {
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
}


final case class PartitionedAlignment(moved: Set[Match], notMoved: Set[Match])
