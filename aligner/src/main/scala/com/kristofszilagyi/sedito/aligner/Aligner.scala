package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.WordIndexRange

import scala.annotation.tailrec

object Aligner {

  //todo test
  @tailrec
  private def extend(currentIdx: Int, words: IndexedSeq[WordIndexRange], offset: Int, result: String): String = {
    if (currentIdx >= 0 && currentIdx < words.size && result.length < math.abs(offset)) {
      val direction = math.signum(offset)
      val word = words(currentIdx).toWord //if this fails that's a bug
      val newResult = if (direction > 0) result + word else word + result
      extend(currentIdx + direction, words, offset, newResult)
    } else {
      result
    }
  }

  //this would be private but I wanted to test it
  private[aligner] def context(fromIdx: Int, words: IndexedSeq[WordIndexRange], offset: Int): String = {
    extend(fromIdx + math.signum(offset), words, offset, result = "")
  }

//  private val ldCalculator = new Levenshtein()
//
//  final case class PairwiseMetrics(ld: Double, ldLenSimilarity: Double)
//
//  private def calcMetrics(left: String, right: String) = {
//    val ld = ldCalculator.distance(left, right)
//    val ldLenSim = LdLenSimilarity.calc(left, right)
//    PairwiseMetrics(ld, ldLenSim)
//  }



//  def align(left: String, right: String): WordAlignment = {
//    val contextSize = 100
//    val leftWords = Wordizer.toWordIndices(left)
//    val rightWords = Wordizer.toWordIndices(right)
//    leftWords.zipWithIndex map { case (leftWord, leftIdx) =>
//      rightWords.zipWithIndex map { case (rightWord, rightIdx) =>
//        val wordMetrics = calcMetrics(leftWord.toWord, rightWord.toWord)
//        val leftBeforeContext = context(leftIdx, leftWords, - contextSize)
//        val leftAfterContext = context(leftIdx, leftWords, contextSize)
//        val rightBeforeContext = context(rightIdx, rightWords, - contextSize)
//        val rightAfterContext = context(rightIdx, rightWords, contextSize)
//        val beforeContextMetrics = calcMetrics(leftBeforeContext, rightBeforeContext)
//        val afterContextMetrics = calcMetrics(leftAfterContext, rightAfterContext)
//        ???
//      }
//    }
//  }
}
