package com.kristofszilagyi.sedito.common

import com.kristofszilagyi.sedito.common.ValidatedOps.RichValidated

object Wordizer {

  def toWordIndices(s: String): Seq[WordIndexRange] = {
    if (s.isEmpty) Seq.empty
    else {
      val separatorIndexes = raw"((?<=[^\w])|(?=[^\w]))".r.findAllMatchIn(s).toList.map(_.start)
      val unfiltered = (0 +: separatorIndexes :+ s.length).sliding(2).toList.flatMap {
        case List(a, b) =>
          if (a < b) Some(WordIndexRange.create(a, b, s).getAssert("invalid range")).toList
          else None.toList
        case _ => None.toList
      }
      unfiltered.map{ idx =>
        (s.slice(idx.startIncl, idx.endExcl), idx)
      }.filterNot{ case (word, _) => word.matches(raw"\s*")}.map(_._2)
    }
  }

  def toWords(s: String): Seq[String] = {
    val indices = toWordIndices(s)
    indices.map{ idx =>
      s.slice(idx.startIncl, idx.endExcl)
    }
  }
}
