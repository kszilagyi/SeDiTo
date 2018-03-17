package com.kristofszilagyi.sedito.common

object Wordizer {

  def toWords(s: String): Seq[String] = {
    if (s.isEmpty) Seq.empty
    else {
      raw"((?<=[^\w])|(?=[^\w]))".r.split(s).filterNot(_.matches(raw"\s+"))
    }
  }
}
