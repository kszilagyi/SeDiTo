package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common._
import info.debatty.java.stringsimilarity.Levenshtein
import org.log4s.getLogger
import TypeSafeEqualsOps._
import scala.annotation.tailrec

object MetricCalculator {
  private val logger = getLogger

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

  final case class PairwiseMetrics(ld: Double, ldLenSim: Double) {
    override def toString: String = s"ld = $ld, ldLenSim = $ldLenSim"
  }

  object NormalizedLenLenSims {
    def calcOne(maxLen: Int, contextSize: Int, ldLenSim: Double): Double = {
      if (maxLen !=== 0) {
        ldLenSim / maxLen * contextSize
      } else contextSize.toDouble /* if they are both on edge their corresponding context is 100% similar */
    }
  }
  /**
    *  this two corrects for the effects of the edges (the context is less than 100 chars)
    */
  final case class NormalizedLenLenSims(before: Double, after: Double)

  final case class ContextMetrics(before: PairwiseMetrics, after: PairwiseMetrics,
                                  normalizedLenLenSims: NormalizedLenLenSims) {
    def toLdLenSimDouble: List[Double] = {
      List(before.ldLenSim, after.ldLenSim, normalizedLenLenSims.before, normalizedLenLenSims.after)
    }

    def toOnlyNormalized: List[Double] = {
      List(normalizedLenLenSims.before, normalizedLenLenSims.after)
    }
  }

  final case class Metrics(leftWord: Selection, rightWord: Selection, word: PairwiseMetrics,
                           contextFull: ContextMetrics, contextHalf: ContextMetrics, contextQuarter: ContextMetrics,
                           context8th: ContextMetrics, context16th: ContextMetrics) {

    def toLdLenSimDouble: Array[Double]= {
      (word.ldLenSim +: (contextFull.toLdLenSimDouble ++ contextHalf.toOnlyNormalized ++
        contextQuarter.toOnlyNormalized ++ context8th.toOnlyNormalized ++ context16th.toOnlyNormalized)).toArray
    }
  }

  private def calcMetrics(left: String, right: String) = {
    val ld = ldCalculator.distance(left, right)
    val ldLenSim = LdLenSimilarity.calcFast(ld, left, right)
    PairwiseMetrics(ld, ldLenSim)
  }

  private def calcContextMetrics(leftWord: WordWithContext, rightWord: WordWithContext, contextSize: Int) = {
    val beforeContextMetrics = calcMetrics(leftWord.beforeContext, rightWord.beforeContext)
    val afterContextMetrics = calcMetrics(leftWord.afterContext, rightWord.afterContext)
    val normalizedLenLenSimBefore = NormalizedLenLenSims.calcOne(
      maxLen = math.max(leftWord.beforeContext.length, rightWord.beforeContext.length), contextSize = contextSize, ldLenSim = beforeContextMetrics.ldLenSim)
    val normalizedLenLenSimAfter = NormalizedLenLenSims.calcOne(
      maxLen = math.max(leftWord.afterContext.length, rightWord.afterContext.length), contextSize = contextSize, ldLenSim = afterContextMetrics.ldLenSim)
    ContextMetrics(beforeContextMetrics, afterContextMetrics, NormalizedLenLenSims(normalizedLenLenSimBefore, normalizedLenLenSimAfter) )
  }
  //concat all words
  //just arithmetic operation from the beginning to end, eithe substring or  CharBuffer.wrap(string).subSequence(from, to)
  private def calcAllMetrics(leftWord: WordWithContext, rightWord: WordWithContext, contextSize: Int) = {
    val wordMetrics = calcMetrics(leftWord.word.toWord, rightWord.word.toWord)
    val contextMetrics = if(wordMetrics.ldLenSim >= 0.99) {
      Some((
        calcContextMetrics(leftWord, rightWord, contextSize),
        calcContextMetrics(leftWord.shortedContext(contextSize / 2), rightWord.shortedContext(contextSize / 2), contextSize),
        calcContextMetrics(leftWord.shortedContext(contextSize / 4), rightWord.shortedContext(contextSize / 4), contextSize),
        calcContextMetrics(leftWord.shortedContext(contextSize / 8), rightWord.shortedContext(contextSize / 8), contextSize),
        calcContextMetrics(leftWord.shortedContext(contextSize / 16), rightWord.shortedContext(contextSize / 16), contextSize)
      ))
    } else {
      None
    }
    contextMetrics.map { case (full, half, quarter, eight, sixteenth) =>
      Metrics(leftWord.word.toSelection, rightWord.word.toSelection, word = wordMetrics,
        contextFull = full, contextHalf = half, contextQuarter = quarter, eight, sixteenth)
    }.toList
  }

  final case class WordWithContext(beforeContext: String, afterContext: String, word: WordIndexRange) {
    def positionAgnostic: (String, String, String) = (beforeContext, afterContext, word.toWord)
    def shortedContext(len: Int): WordWithContext = {
      WordWithContext(beforeContext.take(len), afterContext.take(len), word)
    }
  }


  def calcAlignerMetrics(left: String, right: String): IndexedSeq[Metrics] = {
    val contextSize = 100
    val leftWords = Wordizer.toWordIndices(left)
    val rightWords = Wordizer.toWordIndices(right)
    logger.debug(s"leftWords: $leftWords")
    logger.debug(s"rightWords: $rightWords")
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
        calcAllMetrics(leftWord, rightWord, contextSize)
      }
    }
  }
}
