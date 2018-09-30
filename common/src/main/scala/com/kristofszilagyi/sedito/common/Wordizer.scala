package com.kristofszilagyi.sedito.common

import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated

object Wordizer {
  final case class LineAndPos(line: String, pos: Int)

  def calculateLines(s: String): IndexedSeq[LineAndPos] = {
    val lines = s.linesWithSeparators.toIndexedSeq
    @SuppressWarnings(Array(Warts.TraversableOps))
    val linePositions = lines.foldLeft(IndexedSeq(0)) { case (acc, line) =>
      acc :+ (acc.last + line.length)
    }
    lines.zip(linePositions).map((LineAndPos.apply _).tupled)
  }
  def toWordIndices(s: String): IndexedSeq[Selection] = {
    toWordIndicesWithWhitespaces(s).filterNot{ sel => sel.toText.matches(raw"\s*")}
  }

  def toWordIndicesWithWhitespaces(s: String): IndexedSeq[Selection] = {
    if (s.isEmpty) IndexedSeq.empty
    else {
      calculateLines(s).zipWithIndex.flatMap { case (LineAndPos(line, linePos), lineIdx) =>
        val separatorIndexes = raw"((?<=[^\w])|(?=[^\w]))".r.findAllMatchIn(line).toList.map(_.start)
        val positions = (0 +: separatorIndexes :+ line.length).sliding(2).toVector.flatMap {
          case List(a, b) =>
            if (a < b) Some((a, b)).toList
            else None.toList
          case _ => None.toList
        }

        positions.map { case (start, end) =>
          Selection.create(line, LineIdx(lineIdx), from = CharIdxInLine(start), toExcl = CharIdxInLine(end),
            absoluteFrom = linePos + start).getAssert("invalid range")
        }
      }
    }
  }

  def toWords(s: String): Seq[String] = {
    val indices = toWordIndices(s)
    indices.map(_.toText)
  }

  def toWordsAndWhiteSpaces(s: String): Seq[String] = {
    val indices = toWordIndicesWithWhitespaces(s)
    indices.map(_.toText)
  }
}
