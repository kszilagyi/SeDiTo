package com.kristofszilagyi.sedito.common

import info.debatty.java.stringsimilarity.Levenshtein


/**
  * The metric: len/(ld + 1) - ld/(ld+1)
  * The way to arrive to it:
  * when ld = len => value = 0 (all of them changed they are not similar)
  * y = 1 when ld = 0 and length = 1
  * and y = 1 when len = ld*2 + 1 (at least 1 more than half of them should be not edited to be considered as similar as a single letter to itself)
  * so mathematically:
  * a*len+b = y
  * 1. len = 2ld + 1; ds = 1
  * 2. ld = len; ds = 0
  * and just solve it from here
  *
  * we calculate len as the max of lens because we should imagine the shorter string to be padded to be as long
  * as the longer one (with wrong characters)
  */
object LdLenSimilarity {
  def calc(left: String, right: String): Double = {
    val ld = new Levenshtein().distance(left, right)
    calcFast(ld, left, right)
  }

  /**
    * Only for optimisation - doesn't need to calculate ld again
    */
  def calcFast(ld: Double, left: String, right: String): Double = {
    val maxLen = math.max(left.length, right.length).toDouble
    maxLen / (ld + 1.0) - ld / (ld + 1.0)
  }
}

