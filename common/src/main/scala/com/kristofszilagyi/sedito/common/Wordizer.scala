package com.kristofszilagyi.sedito.common

import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import com.kristofszilagyi.sedito.common.Warts.{discard}
import TypeSafeEqualsOps._

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

  private def isWord(c: Char) = c.isLetterOrDigit || c ==== '_'
  // rewritten to be non-functional for performance
  @SuppressWarnings(Array(Warts.Var, Warts.While))
  def toWordIndicesWithWhitespaces(s: String): IndexedSeq[Selection] = {
    if (s.isEmpty) IndexedSeq.empty
    else {
      calculateLines(s).zipWithIndex.flatMap { case (LineAndPos(line, linePos), lineIdx) =>
        val positions = Vector.newBuilder[(Int, Int)]

        if (line.nonEmpty) {
          var i = 1
          var wordStart = 0
          var singleLetterWord = !isWord(line.charAt(0))
          while (i < line.length) {
            val currentChar = line.charAt(i)

            if (!isWord(currentChar)) {
              discard(positions += ((wordStart, i)))
              wordStart = i
              singleLetterWord = true
            } else if (singleLetterWord) {
              discard(positions += ((wordStart, i)))
              wordStart = i
              singleLetterWord = false
            }
            i += 1
          }
          discard(positions += ((wordStart, line.length)))
        }
        positions.result().map { case (start, end) =>
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
