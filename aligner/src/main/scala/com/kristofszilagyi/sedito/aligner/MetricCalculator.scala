package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps.AnyOps
import com.kristofszilagyi.sedito.common._
import info.debatty.java.stringsimilarity.Levenshtein
import org.log4s.getLogger

import scala.annotation.tailrec

object MetricCalculator {
  import com.kristofszilagyi.sedito.aligner.MetricCalculator.ClosestContext._

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
  final case class PairwiseMetrics(ldSim: Double, ldSimEdgeAdjusted: Double, normalizedLd: Double, normalizedLdLenSim: Double, ldLenSim: Double) {
    assert(ldSim >= 0)
    assert(ldSimEdgeAdjusted >= 0)
    assert(normalizedLd >= 0 && normalizedLd <= 1)
    assert(normalizedLdLenSim >= 0)
    assert(ldLenSim >= 0)
    override def toString: String = s"ldSim = $ldSim, ldSimEdgeAdjusted = $ldSimEdgeAdjusted, " +
      s"normalizedLd = $normalizedLd, normalizedLdLenSim = $normalizedLdLenSim, ldLenSim = $ldLenSim"

    def toDoubles: List[Double] = List(ldSim, ldSimEdgeAdjusted, normalizedLd, normalizedLdLenSim, ldLenSim)
  }


  final case class ContextMetrics(before: PairwiseMetrics, after: PairwiseMetrics) {
    def doubles: List[Double] = {
      before.toDoubles ++ after.toDoubles
    }

    override def toString: String = s"(b: $before,a: $after)"
  }

  final case class Phase1Metrics(sameLineSameWord: Double, leftWord: Selection, rightWord: Selection,
                                         word: PairwiseMetrics, line: PairwiseMetrics, contextFull: ContextMetrics,
                                         context4th: ContextMetrics, context8th: ContextMetrics, context16th: ContextMetrics,
                                         leftLineIdx: LineIdx, rightLineIdx: LineIdx)


  final case class ContextIsClosest(beforeFromLeft: Boolean, beforeFromRight: Boolean, afterFromLeft: Boolean, afterFromRight: Boolean) {
    override def toString: String = s"CIC(bL: $beforeFromLeft, bR: $beforeFromRight, aL: $afterFromLeft, aR: $afterFromRight)"

    def doubles: Seq[Double] = Seq(beforeFromLeft, beforeFromRight, afterFromLeft, afterFromRight).map(if (_) 1.0 else 0.0)
  }
  final case class Metrics(phase1Metrics: Phase1Metrics,
                           lineIsClosestMatchInText: Boolean,
                           closestFull: ContextIsClosest,
                           closest4th: ContextIsClosest,
                           closest8th: ContextIsClosest,
                           closest16th: ContextIsClosest) {

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
        context8th.doubles ++ context16th.doubles ++ closestFull.doubles ++ closest4th.doubles
        ++ closest8th.doubles ++ closest16th.doubles :+ (if (lineIsClosestMatchInText) 1.0 else 0.0))).toArray
    }

    override def toString: String = {
      s"$leftWord - $rightWord: ss: $sameLineSameWord, word: $word, full: $contextFull," +
        s" 4: $context4th, 16: $context16th, lineIsClosest: $lineIsClosestMatchInText"
    }
  }

  private def calcMetrics(left: String, right: String, supposedMaxLength: Int) = {
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
    val maxPossibleLength = math.max(supposedMaxLength, maxLen)
    val ldSimEdgeAdjusted = maxPossibleLength - ld // the logic being that where there was no string they are the same
    PairwiseMetrics(ldSim, ldSimEdgeAdjusted, normalizedLd, normalizedSim, ldLenSim)
  }

  private def calcContextMetrics(leftWord: WordWithContext, rightWord: WordWithContext, contextSize: Int) = {
    val beforeContextMetrics = calcMetrics(leftWord.beforeContext, rightWord.beforeContext, contextSize)
    val afterContextMetrics = calcMetrics(leftWord.afterContext, rightWord.afterContext, contextSize)
    ContextMetrics(beforeContextMetrics, afterContextMetrics)
  }

  private def calcShortenedContextMetrics(leftWord: WordWithContext, rightWord: WordWithContext, contextSize: Int) = {
    calcContextMetrics(leftWord.shortedContext(contextSize), rightWord.shortedContext(contextSize), contextSize)
  }

  //concat all words
  //just arithmetic operation from the beginning to end, eithe substring or  CharBuffer.wrap(string).subSequence(from, to)
  private def calcAllMetrics(leftWord: WordWithContext, rightWord: WordWithContext, contextSize: Int) = {
    val leftWordString = leftWord.word.toWord
    val rightWordString = rightWord.word.toWord
    val wordMetrics = calcMetrics(leftWordString, rightWordString, math.max(leftWordString.length, rightWordString.length))
    val leftSelection = leftWord.word.toSelection
    val leftLine = leftSelection.line
    val rightSelection = rightWord.word.toSelection
    val rightLine = rightSelection.line
    val lineMetrics = calcMetrics(leftLine, rightLine, math.max(leftLine.length, rightLine.length))//todo this could be cached/sped up
    val sameLineSameWord = if (leftLine ==== rightLine && leftSelection.from ==== rightSelection.from) 1.0
                        else 0.0
    val contextMetrics = if(wordMetrics.ldLenSim >= 0.99) {
      Some((
        calcContextMetrics(leftWord, rightWord, contextSize),
        calcShortenedContextMetrics(leftWord, rightWord, contextSize / 4),
        calcShortenedContextMetrics(leftWord, rightWord, contextSize / 8),
        calcShortenedContextMetrics(leftWord, rightWord, contextSize / 16)
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

  /**
    * Position agnostic - as opposed to WordIndexRange which has position in it
    */
  final case class WordWithContextPositionAgnostic(before: String, after: String, word: String)
  final case class WordWithContext(beforeContext: String, afterContext: String, word: WordIndexRange) {
    def positionAgnostic: WordWithContextPositionAgnostic = WordWithContextPositionAgnostic(beforeContext, afterContext, word.toWord)
    def shortedContext(len: Int): WordWithContext = {
      WordWithContext(beforeContext.takeRight(len), afterContext.take(len), word)
    }
  }

  private def findClosestLines(potentials: Map[_, Traversable[Phase1Metrics]]) = {
    potentials flatMap { case (_, oneGroup) =>
      //we know this will never throw because it doesn't make sense to have empty collection on the right side of the map
      val closest = oneGroup.minBy(_.line.normalizedLd)
      val closestNormalizedLd = closest.line.normalizedLd
      val allClosest = oneGroup.filter(p => math.abs(p.line.normalizedLd - closestNormalizedLd) < 0.0001)
      val ambiguous = allClosest.map(m => (m.leftLineIdx, m.rightLineIdx)).toSet.size > 1
      if (ambiguous) Traversable.empty
      else allClosest
    }
  }

  private def calcClosestLineMatches(phase1Metrics: IndexedSeq[Phase1Metrics]) = {
    val leftWordPotentials = phase1Metrics.groupBy(_.leftWord)
    val rightWordPotentials = phase1Metrics.groupBy(_.rightWord)
    val closestFromLeft = findClosestLines(leftWordPotentials)
    val closestFromRight = findClosestLines(rightWordPotentials)

    val unresolvedClosests = (closestFromLeft ++ closestFromRight).toSet
    val leftLineConflicts = unresolvedClosests.groupBy(_.leftLineIdx)
    val rightLineConflicts = unresolvedClosests.groupBy(_.rightLineIdx)
    val resolvedFromLeft = findClosestLines(leftLineConflicts)
    val resolvedFromRight = findClosestLines(rightLineConflicts)

    val conflictingOnesOnLeft = unresolvedClosests -- resolvedFromLeft
    val conflictingOnesOnRight = unresolvedClosests -- resolvedFromRight

    unresolvedClosests -- conflictingOnesOnLeft -- conflictingOnesOnRight

  }

  private object ClosestContext {
    final case class ClosestContextMatches(beforeLeft: Set[Phase1Metrics], beforeRight: Set[Phase1Metrics],
                                           afterLeft: Set[Phase1Metrics], afterRight: Set[Phase1Metrics]) {
      def in(phase1Metrics: Phase1Metrics): ContextIsClosest = {
        ContextIsClosest(beforeFromLeft = beforeLeft.contains(phase1Metrics), beforeFromRight = beforeRight.contains(phase1Metrics),
        afterFromLeft = afterLeft.contains(phase1Metrics), afterFromRight = afterRight.contains(phase1Metrics))
      }
    }
    private def findClosestContext(potentials: Map[_, Traversable[Phase1Metrics]], contextSelector: Phase1Metrics => Double) = {
      potentials flatMap { case (_, oneGroup) =>
        //we know this will never throw because it doesn't make sense to have empty collection on the right side of the map
        val closest = oneGroup.maxBy(contextSelector)
        val closestValue = contextSelector(closest)
        val allClosest = oneGroup.filter(p => math.abs(contextSelector(p) - closestValue) < 0.0001)
        val ambiguous = allClosest.toSet.size > 1
        if (ambiguous) Traversable.empty
        else allClosest
      }
    }

    private def findClosestForSide(leftPotentials: Map[_, Traversable[Phase1Metrics]], rightPotentials: Map[_, Traversable[Phase1Metrics]],
                                   contextSelector: Phase1Metrics => PairwiseMetrics) = {
      val closestFromLeft = findClosestContext(leftPotentials, p => contextSelector(p).ldSimEdgeAdjusted)
      val closestFromRight = findClosestContext(rightPotentials, p => contextSelector(p).ldSimEdgeAdjusted)
      (closestFromLeft, closestFromRight)
    }

    def calcClosestContextMatches(phase1Metrics: IndexedSeq[Phase1Metrics], contextSelector: Phase1Metrics => ContextMetrics): ClosestContextMatches = {
      //So I found it too hard to do something I have done for line (resolving conflicts)
      //so I realised I could just not do that and do a feature based on both sides. It's simpler and potentially more info (or less, not sure)
      val leftWordPotentials = phase1Metrics.groupBy(_.leftWord)
      val rightWordPotentials = phase1Metrics.groupBy(_.rightWord)
      val (beforeLeft, beforeRight) = findClosestForSide(leftWordPotentials, rightWordPotentials, m => contextSelector(m).before)
      val (afterLeft, afterRight) = findClosestForSide(leftWordPotentials, rightWordPotentials, m => contextSelector(m).after)
      ClosestContextMatches(
        beforeLeft = beforeLeft.toSet, beforeRight = beforeRight.toSet,
        afterLeft = afterLeft.toSet, afterRight = afterRight.toSet
      )
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
    logger.debug(s"Metrics: ${phase1Metrics.mkString("\n")}")
    val closestLineMatches = calcClosestLineMatches(phase1Metrics)
    val closestContextFull = calcClosestContextMatches(phase1Metrics, _.contextFull)
    val closestContext4th = calcClosestContextMatches(phase1Metrics, _.context4th)
    val closestContext8th = calcClosestContextMatches(phase1Metrics, _.context8th)
    val closestContext16th = calcClosestContextMatches(phase1Metrics, _.context16th)
    phase1Metrics.map{m =>
      val closest = closestLineMatches.contains(m)
      Metrics(m, lineIsClosestMatchInText = closest, closestFull = closestContextFull.in(m), closest4th = closestContext4th.in(m),
        closest8th = closestContext8th.in(m), closest16th = closestContext16th.in(m))
    }
  }
}
