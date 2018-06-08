package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps.AnyOps
import com.kristofszilagyi.sedito.common._
import info.debatty.java.stringsimilarity.Levenshtein
import org.log4s.getLogger

import scala.annotation.tailrec

object MetricCalculator {
  private val logger = getLogger

  @tailrec
  private def extend(currentIdx: Int, words: IndexedSeq[WordIndexRange], offset: Int, result: List[String], resultLength: Int): String = {
    if (currentIdx >= 0 && currentIdx < words.size && resultLength < math.abs(offset)) {
      val direction = math.signum(offset)
      val word = words(currentIdx).toWord //if this fails that's a bug
      val newResult = word +: result
      extend(currentIdx + direction, words, offset, newResult, resultLength + word.length)
    } else {
      if (offset < 0) result.mkString
      else result.reverse.mkString
    }
  }

  //this would be private but I wanted to test it
  private[aligner] def context(fromIdx: Int, words: IndexedSeq[WordIndexRange], offset: Int): String = {
    extend(fromIdx + math.signum(offset), words, offset, result = List.empty, resultLength = 0)
  }

  private val ldCalculator = new Levenshtein()

  /**
    * @param normalizedLdLenSim 1 if they are the same.
    * @param ldLenSim
    */
  final case class PairwiseMetrics(normalizedLdLenSim: Double, ldLenSim: Double) {
    override def toString: String = s"normalizedLdLenSim = $normalizedLdLenSim, ldLenSim = $ldLenSim"

    def toDoubles: List[Double] = List(normalizedLdLenSim, ldLenSim)
  }

  /**
    *  this two corrects for the effects of the edges (the context is less than 100 chars)
    */
  final case class NormalizedLenLenSims(before: Double, after: Double)

  final case class WordMetrics(sameWords: Int, sameWordsNormalized: Double) {
    def normalizedDoubles: List[Double] = List(sameWordsNormalized)
    def doubles: List[Double] = List(sameWords.toDouble) ++ normalizedDoubles
  }

  final case class ContextMetrics(before: PairwiseMetrics, after: PairwiseMetrics, beforeWords: WordMetrics, afterWords: WordMetrics) {
    def toLdLenSimDouble: List[Double] = {
      before.toDoubles ++ after.toDoubles ++ beforeWords.doubles ++ afterWords.doubles
    }

    def toOnlyNormalized: List[Double] = {
      List(before.normalizedLdLenSim, after.normalizedLdLenSim) ++ beforeWords.normalizedDoubles ++ afterWords.normalizedDoubles
    }
  }

  final case class Metrics(sameLineSameWord: Double, leftWord: Selection, rightWord: Selection, word: PairwiseMetrics, line: PairwiseMetrics,
                           contextFull: ContextMetrics, context4th: ContextMetrics, context8th: ContextMetrics, context16th: ContextMetrics) {

    def toLdLenSimDouble: Array[Double]= {
      (sameLineSameWord +: (word.toDoubles ++ line.toDoubles ++ contextFull.toLdLenSimDouble ++ context4th.toOnlyNormalized ++
        context8th.toOnlyNormalized ++context16th.toOnlyNormalized)).toArray
    }

    override def toString: String = {
      s"${leftWord} - ${rightWord}: ss: $sameLineSameWord, word: ${word.ldLenSim}, ${word.normalizedLdLenSim}, " +
        s"full: ${contextFull.toLdLenSimDouble.mkString(", ")}," +
        s" 16: ${context16th.toOnlyNormalized.mkString(", ")}"
    }
  }

  private def calcMetrics(left: String, right: String) = {
    val ld = ldCalculator.distance(left, right)
    val ldLenSim = LdLenSimilarity.calcFast(ld, left, right)
    val maxLen = math.max(left.length, right.length)
    val normalizedSim = if (maxLen > 0) {
       ldLenSim / maxLen
    } else {
      1.0
    }
    PairwiseMetrics(normalizedSim, ldLenSim)
  }

  private def calcWordMetrics(left: String, right: String) = {
    val leftWords = Wordizer.toWords(left)
    val rightWords = Wordizer.toWords(right)
    val wordCount = math.max(leftWords.size, rightWords.size)
    val commonWordsCount = leftWords.toSet.intersect(rightWords.toSet).size
    WordMetrics(commonWordsCount, if (wordCount !=== 0) commonWordsCount.toDouble / wordCount.toDouble else 1)
  }

  private def calcContextMetrics(leftWord: WordWithContext, rightWord: WordWithContext) = {
    val beforeContextMetrics = calcMetrics(leftWord.beforeContext, rightWord.beforeContext)
    val afterContextMetrics = calcMetrics(leftWord.afterContext, rightWord.afterContext)
    val beforeWordContextMetrics = calcWordMetrics(leftWord.beforeContext, rightWord.beforeContext)
    val afterWordContextMetrics = calcWordMetrics(leftWord.afterContext, rightWord.afterContext)
    ContextMetrics(beforeContextMetrics, afterContextMetrics, beforeWordContextMetrics, afterWordContextMetrics)
  }
  //concat all words
  //just arithmetic operation from the beginning to end, eithe substring or  CharBuffer.wrap(string).subSequence(from, to)
  private def calcAllMetrics(leftWord: WordWithContext, rightWord: WordWithContext, contextSize: Int) = {
    val wordMetrics = calcMetrics(leftWord.word.toWord, rightWord.word.toWord)
    val leftSelection = leftWord.word.toSelection
    val leftLine = leftSelection.line
    val rightSelection = rightWord.word.toSelection
    val rightLine = rightSelection.line
    val lineMetrics = calcMetrics(leftLine, rightLine)//todo this could be cached/sped up
    val sameLineSameWord = if (leftLine ==== rightLine && leftSelection.from ==== rightSelection.from) 1.0
                        else 0.0
    val contextMetrics = if(wordMetrics.ldLenSim >= 0.99) {
      Some((
        calcContextMetrics(leftWord, rightWord),
        calcContextMetrics(leftWord.shortedContext(contextSize / 4), rightWord.shortedContext(contextSize / 4)),
        calcContextMetrics(leftWord.shortedContext(contextSize / 8), rightWord.shortedContext(contextSize / 8)),
        calcContextMetrics(leftWord.shortedContext(contextSize / 16), rightWord.shortedContext(contextSize / 16))
      ))
    } else {
      None
    }
    contextMetrics.map { case (full, forth, eight, sixteenth) =>
      Metrics(sameLineSameWord, leftWord.word.toSelection, rightWord.word.toSelection, word = wordMetrics, line = lineMetrics,
        contextFull = full, context4th = forth, context8th = eight,  context16th = sixteenth)
    }.toList
  }

  final case class WordWithContext(beforeContext: String, afterContext: String, word: WordIndexRange) {
    def positionAgnostic: (String, String, String) = (beforeContext, afterContext, word.toWord)
    def shortedContext(len: Int): WordWithContext = {
      WordWithContext(beforeContext.takeRight(len), afterContext.take(len), word)
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
