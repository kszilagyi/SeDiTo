package com.kristofszilagyi.sedito.common

import cats.data.Validated.{Invalid, Valid}
import com.kristofszilagyi.sedito.common.AssertionEx.fail

object Wordizer {

  def toWordIndices(s: String): Seq[WordIndexRange] = {
    if (s.isEmpty) Seq.empty
    else {
      val separatorIndexes = raw"((?<=[^\w])|(?=[^\w]))".r.findAllMatchIn(s).toList.map(_.start)
      val unfiltered = (0 +: separatorIndexes :+ s.length).sliding(2).toList.flatMap {
        case List(a, b) =>
          WordIndexRange.create(a, b, s) match {
            case Valid(a) => Some(a).toList
            case Invalid(e) => fail(s"Bug in code: invalid range: $e")
          }
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
