package com.kristofszilagyi.sedito.common

import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated

object Wordizer {

  def toWordIndices(s: String): IndexedSeq[WordIndexRange] = {
    if (s.isEmpty) IndexedSeq.empty
    else {
      val separatorIndexes = raw"((?<=[^\w])|(?=[^\w]))".r.findAllMatchIn(s).toList.map(_.start)
      val unfiltered = (0 +: separatorIndexes :+ s.length).sliding(2).toVector.flatMap {
        case List(a, b) =>
          if (a < b) Some((a, b)).toList
          else None.toList
        case _ => None.toList
      }
      val filtered = unfiltered.map{ case (start, end) =>
        (s.slice(start, end), (start, end))
      }.filterNot{ case (word, _) => word.matches(raw"\s*")}.map(_._2)
      val fullText = FullText(s)
      filtered.map{ case (start, end) =>
        WordIndexRange.create(startIncl = start, endExcl = end, fullText = fullText).getAssert("invalid range")
      }
    }
  }

  def toWords(s: String): Seq[String] = {
    val indices = toWordIndices(s)
    indices.map{ idx =>
      s.slice(idx.startIncl, idx.endExcl)
    }
  }
}
