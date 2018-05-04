package com.kristofszilagyi.sedito.common.utils

import scala.collection.immutable.Map

object MapOps {
  implicit class RichMap[K, V](m: Map[K, V]) {
    def mapValuesNow[W](f: V => W): Map[K, W] = {
      m.map { case (k, v) =>
        k -> f(v)
      }
    }
  }
}
