package com.kristofszilagyi.sedito.common

import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated
import com.kristofszilagyi.sedito.common.Warts.discard
import TypeSafeEqualsOps._

import scala.collection.mutable

object Wordizer {
  final case class LineAndPos(line: String, pos: Int)

  def calculateLines(s: String): IndexedSeq[LineAndPos] = {
    val lines = s.linesWithSeparators.toIndexedSeq
    val linePositions = Vector.newBuilder[Int]
    linePositions.sizeHint(lines.size)
    discard(linePositions += 0)
    @SuppressWarnings(Array(Warts.Var))
    var lastPos = 0
    lines.foreach { case (line) =>
      lastPos += line.length
      discard(linePositions += lastPos)
    }
    lines.zip(linePositions.result()).map((LineAndPos.apply _).tupled)
  }

  private def isWord(c: Char) = c.isLetterOrDigit || c ==== '_'
  private def addNonWhiteSpace(builder: mutable.Builder[(Int, Int), Vector[(Int, Int)]], start: Int, end: Int, line: String): Unit = {
    if (end - start > 1 || !line.charAt(start).isWhitespace) {
      discard(builder += ((start, end)))
    }
  }
  // rewritten to be non-functional for performance
  @SuppressWarnings(Array(Warts.Var, Warts.While))
  def toWordIndices(s: String): IndexedSeq[Selection] = {
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
              addNonWhiteSpace(positions, wordStart, i, line)
              wordStart = i
              singleLetterWord = true
            } else if (singleLetterWord) {
              addNonWhiteSpace(positions, wordStart, i, line)
              wordStart = i
              singleLetterWord = false
            }
            i += 1
          }
          addNonWhiteSpace(positions, wordStart, line.length, line)
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

}
