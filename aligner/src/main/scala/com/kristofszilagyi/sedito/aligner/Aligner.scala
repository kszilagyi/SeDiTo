package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common._
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
  //concat all words
  //just arithmetic operation from the beginning to end, eithe substring or  CharBuffer.wrap(string).subSequence(from, to)
  private def calcAllMetrics(leftWord: WordWithContext, rightWord: WordWithContext) = {
    val wordMetrics = calcMetrics(leftWord.word.toWord, rightWord.word.toWord)
    val contextMetrics = if(wordMetrics.ldLenSimilarity >= 0.99) {
      val beforeContextMetrics = calcMetrics(leftWord.beforeContext, rightWord.beforeContext)
      val afterContextMetrics = calcMetrics(leftWord.afterContext, rightWord.afterContext)
      Some(ContextMetrics(beforeContextMetrics, afterContextMetrics))
    } else {
      None
    }
    contextMetrics.map { c =>
      Metrics(leftWord.word.toSelection, rightWord.word.toSelection, word = wordMetrics, c)
    }.toList
  }

  final case class WordWithContext(beforeContext: String, afterContext: String, word: WordIndexRange) {
    def positionAgnostic: (String, String, String) = (beforeContext, afterContext, word.toWord)
  }

  def calcAlignerMetrics(left: String, right: String): IndexedSeq[Metrics] = {
    val contextSize = 100
    val leftWords = Wordizer.toWordIndices(left)
    val rightWords = Wordizer.toWordIndices(right)
    val leftContexts = leftWords.zipWithIndex map { case (word, leftIdx) =>
      val leftBeforeContext = context(leftIdx, leftWords, -contextSize)
      val leftAfterContext = context(leftIdx, leftWords, contextSize)
      WordWithContext(leftBeforeContext, leftAfterContext, word)
    }

    val rightContexts = rightWords.zipWithIndex map { case (word, rightIdx) =>
      val rightBeforeContext = context(rightIdx, rightWords, -contextSize)
      val rightAfterContext = context(rightIdx, rightWords, contextSize)
      WordWithContext(rightBeforeContext, rightAfterContext, word)
    }

    val candidateCtxFinder = new CandidateFinder(rightContexts.toSet)
    leftContexts.flatMap { leftWord =>
      val candidates = if (leftWord.beforeContext.length < contextSize) {
        rightContexts
      } else if (leftWord.afterContext.length < contextSize) {
        rightContexts
      } else {
        candidateCtxFinder.possibleMatches(leftWord)
      }
      candidates.flatMap { rightWord =>
        calcAllMetrics(leftWord, rightWord)
      }
    }
  }
}
