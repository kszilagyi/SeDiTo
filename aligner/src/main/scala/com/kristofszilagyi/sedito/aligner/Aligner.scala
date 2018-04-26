package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.{LdLenSimilarity, Selection, WordIndexRange, Wordizer}
import info.debatty.java.stringsimilarity.Levenshtein

import scala.annotation.tailrec

object Aligner {
 // private val logger = getLogger

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

  private val ldCalculator = new Levenshtein()

  final case class PairwiseMetrics(ld: Double, ldLenSimilarity: Double)
  final case class ContextMetrics(before: PairwiseMetrics, after: PairwiseMetrics)
  final case class Metrics(leftWord: Selection, rightWord: Selection, word: PairwiseMetrics,
                           context: ContextMetrics) {
    def toDoubles: Array[Double] = {
      Array(word.ld, word.ldLenSimilarity, context.before.ld, context.before.ldLenSimilarity,
        context.after.ld, context.after.ldLenSimilarity)

    }
  }

  private def calcMetrics(left: String, right: String) = {
    val ld = ldCalculator.distance(left, right)
    val ldLenSim = LdLenSimilarity.calcFast(ld, left, right)
    PairwiseMetrics(ld, ldLenSim)
  }

  def calcAlignerMetrics(left: String, right: String): IndexedSeq[Metrics] = {
    val contextSize = 100
    val leftWords = Wordizer.toWordIndices(left)
    val rightWords = Wordizer.toWordIndices(right)
    leftWords.zipWithIndex flatMap { case (leftWord, leftIdx) =>
      rightWords.zipWithIndex flatMap { case (rightWord, rightIdx) =>
        val wordMetrics = calcMetrics(leftWord.toWord, rightWord.toWord)
        val contextMetrics = if(wordMetrics.ldLenSimilarity >= 0.99) {
          val leftBeforeContext = context(leftIdx, leftWords, -contextSize)
          val leftAfterContext = context(leftIdx, leftWords, contextSize)
          val rightBeforeContext = context(rightIdx, rightWords, -contextSize)
          val rightAfterContext = context(rightIdx, rightWords, contextSize)
          val beforeContextMetrics = calcMetrics(leftBeforeContext, rightBeforeContext)
          val afterContextMetrics = calcMetrics(leftAfterContext, rightAfterContext)
          Some(ContextMetrics(beforeContextMetrics, afterContextMetrics))
        } else {
          None
        }
        contextMetrics.map { c =>
          Metrics(leftWord.toSelection, rightWord.toSelection, word = wordMetrics, c)
        }.toList
      }
    }
  }
}
