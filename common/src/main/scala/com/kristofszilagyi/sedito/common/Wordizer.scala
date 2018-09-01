package com.kristofszilagyi.sedito.common

import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated

object Wordizer {

  def toWordIndices(s: String): IndexedSeq[Selection] = {
    if (s.isEmpty) IndexedSeq.empty
    else {
      val lines = s.linesWithSeparators.toIndexedSeq
      @SuppressWarnings(Array(Warts.TraversableOps))
      val linePositions = lines.foldLeft(IndexedSeq(0)) { case (acc, line) =>
        acc :+ (acc.last + line.length)
      }

      lines.zip(linePositions).zipWithIndex.flatMap { case ((line, linePos), lineIdx) =>
        val separatorIndexes = raw"((?<=[^\w])|(?=[^\w]))".r.findAllMatchIn(line).toList.map(_.start)
        val unfiltered = (0 +: separatorIndexes :+ line.length).sliding(2).toVector.flatMap {
          case List(a, b) =>
            if (a < b) Some((a, b)).toList
            else None.toList
          case _ => None.toList
        }
        val filtered = unfiltered.map{ case (start, end) =>
          (line.slice(start, end), (start, end))
        }.filterNot{ case (word, _) => word.matches(raw"\s*")}.map(_._2)
        filtered.map { case (start, end) =>
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
