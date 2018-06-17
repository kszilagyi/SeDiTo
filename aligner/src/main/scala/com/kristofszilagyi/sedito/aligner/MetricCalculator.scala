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
  final case class PairwiseMetrics(ldSim: Double, normalizedLd: Double, normalizedLdLenSim: Double, ldLenSim: Double) {
    override def toString: String = s"ld = $ldSim, normalizedLd = $normalizedLd, normalizedLdLenSim = $normalizedLdLenSim, ldLenSim = $ldLenSim"

    def toDoubles: List[Double] = List(ldSim, normalizedLd, normalizedLdLenSim, ldLenSim)
  }


  final case class ContextMetrics(before: PairwiseMetrics, after: PairwiseMetrics) {
    def doubles: List[Double] = {
      before.toDoubles ++ after.toDoubles
    }
  }

  final case class Phase1Metrics(sameLineSameWord: Double, leftWord: Selection, rightWord: Selection,
                                         word: PairwiseMetrics, line: PairwiseMetrics, contextFull: ContextMetrics,
                                         context4th: ContextMetrics, context8th: ContextMetrics, context16th: ContextMetrics,
                                         leftLineIdx: LineIdx, rightLineIdx: LineIdx)

  final case class Metrics(phase1Metrics: Phase1Metrics,
                           lineIsClosestMatchInText: Boolean) {
    def sameLineSameWord: Double = phase1Metrics.sameLineSameWord
    def word: PairwiseMetrics = phase1Metrics.word
    def line: PairwiseMetrics = phase1Metrics.line
    def contextFull: ContextMetrics = phase1Metrics.contextFull
    def context4th: ContextMetrics = phase1Metrics.context4th
    def context8th: ContextMetrics = phase1Metrics.context8th
    def context16th: ContextMetrics = phase1Metrics.context16th
    def leftWord: Selection = phase1Metrics.leftWord
    def rightWord: Selection = phase1Metrics.rightWord
    def leftLineIdx: LineIdx = phase1Metrics.leftLineIdx
    def rightLineIdx: LineIdx = phase1Metrics.rightLineIdx

    def toLdLenSimDouble: Array[Double]= {
      (sameLineSameWord +: (word.toDoubles ++ line.toDoubles ++ contextFull.doubles ++ context4th.doubles ++
        context8th.doubles ++context16th.doubles :+ (if (lineIsClosestMatchInText) 1.0 else 0.0))).toArray
    }

    override def toString: String = {
      s"$leftWord - $rightWord: ss: $sameLineSameWord, word: ${word.ldLenSim}, ${word.normalizedLdLenSim}, " +
        s"full: ${contextFull.doubles.mkString(", ")}," +
        s" 16: ${context16th.doubles.mkString(", ")}"
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
    val normalizedLd =  if (maxLen > 0) {
      ld / maxLen
    } else {
      1.0
    }
    val ldSim = maxLen - ld // in theory this is better as it has more info than ld
    PairwiseMetrics(ldSim, normalizedLd, normalizedSim, ldLenSim)
  }

  private def calcContextMetrics(leftWord: WordWithContext, rightWord: WordWithContext) = {
    val beforeContextMetrics = calcMetrics(leftWord.beforeContext, rightWord.beforeContext)
    val afterContextMetrics = calcMetrics(leftWord.afterContext, rightWord.afterContext)
    ContextMetrics(beforeContextMetrics, afterContextMetrics)
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
      Phase1Metrics(sameLineSameWord, leftWord.word.toSelection, rightWord.word.toSelection, word = wordMetrics, line = lineMetrics,
        contextFull = full, context4th = forth, context8th = eight,  context16th = sixteenth, leftLineIdx= leftSelection.lineIdx,
        rightLineIdx = rightSelection.lineIdx)
    }.toList
  }

  final case class WordWithContext(beforeContext: String, afterContext: String, word: WordIndexRange) {
    def positionAgnostic: (String, String, String) = (beforeContext, afterContext, word.toWord)
    def shortedContext(len: Int): WordWithContext = {
      WordWithContext(beforeContext.takeRight(len), afterContext.take(len), word)
    }
  }


  def calcAlignerMetrics(left: FullText, right: FullText): IndexedSeq[Metrics] = {
    val contextSize = 100
    val leftWords = Wordizer.toWordIndices(left.s)
    val rightWords = Wordizer.toWordIndices(right.s)
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
    val phase1Metrics = leftContexts.flatMap { leftWord =>
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
    //val leftphase1Metrics.groupBy(_.leftLine)
    phase1Metrics.map(m => Metrics(m, lineIsClosestMatchInText = true))
  }
}
