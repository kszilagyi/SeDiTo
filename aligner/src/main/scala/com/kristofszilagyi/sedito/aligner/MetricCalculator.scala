package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.MetricCalculator.ContextIsClosest._
import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common._
import info.debatty.java.stringsimilarity.Levenshtein
import org.log4s.getLogger

import scala.annotation.tailrec

object MetricCalculator {
  import com.kristofszilagyi.sedito.aligner.MetricCalculator.ClosestContext._

  private val logger = getLogger

  @tailrec
  private def extend(currentIdx: Int, words: IndexedSeq[Selection], offset: Int, result: List[String], resultLength: Int): String = {
    if (currentIdx >= 0 && currentIdx < words.size && resultLength < math.abs(offset)) {
      val direction = math.signum(offset)
      val word = words(currentIdx).toText //if this fails that's a bug
      val newResult = word +: result
      extend(currentIdx + direction, words, offset, newResult, resultLength + word.length)
    } else {
      if (offset < 0) result.mkString
      else result.reverse.mkString
    }
  }

  //this would be private but I wanted to test it
  private[aligner] def context(fromIdx: Int, words: IndexedSeq[Selection], offset: Int): String = {
    extend(fromIdx + math.signum(offset), words, offset, result = List.empty, resultLength = 0)
  }

  private val ldCalculator = new Levenshtein()

  object PairwiseMetrics {
    def columnNames: List[String] = {
      List("ldSim", "ldSimEdgeAdjusted", "normalizedLd", "normalizedLdLenSim", "ldLenSim")
    }
  }
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

    def toDoubles(array: ArrayHolder): Unit = {
      array.add(ldSim)
      array.add(ldSimEdgeAdjusted)
      array.add(normalizedLd)
      array.add(normalizedLdLenSim)
      array.add(ldLenSim)
    }
  }

  object ContextMetrics {
    def columnNames: List[String] = {
      Metrics.withChildren("before", PairwiseMetrics.columnNames) ++ Metrics.withChildren("after", PairwiseMetrics.columnNames)
    }
  }

  final case class ContextMetrics(before: PairwiseMetrics, after: PairwiseMetrics) {
    def doubles(array: ArrayHolder): Unit = {
      before.toDoubles(array)
      after.toDoubles(array)
    }

    override def toString: String = s"(b: $before,a: $after)"
  }

  final case class Phase1Metrics(leftWord: Selection, rightWord: Selection,
                                         word: PairwiseMetrics, line: PairwiseMetrics,
                                         context4th: ContextMetrics, context8th: ContextMetrics, context16th: ContextMetrics,
                                         leftLineIdx: LineIdx, rightLineIdx: LineIdx)


  object ContextIsClosest {
    def columnNames: List[String] = {
      List("beforeFromLeft", "beforeFromRight", "afterFromLeft", "afterFromRight")
    }
    private def toDouble(from: Boolean) = if(from) 1.0 else 0.0
  }
  final case class ContextIsClosest(beforeFromLeft: Boolean, beforeFromRight: Boolean, afterFromLeft: Boolean, afterFromRight: Boolean) {
    override def toString: String = s"CIC(bL: $beforeFromLeft, bR: $beforeFromRight, aL: $afterFromLeft, aR: $afterFromRight)"

    def doubles(array: ArrayHolder): Unit = {
      array.add(toDouble(beforeFromLeft))
      array.add(toDouble(beforeFromRight))
      array.add(toDouble(afterFromLeft))
      array.add(toDouble(afterFromRight))
    }
  }
  final class ArrayHolder(array: Array[Double]) {
    @SuppressWarnings(Array(Warts.Var))
    private var i = 0
    def add(d: Double): Unit = {
      array(i) = d
      i += 1
    }
  }
  object Metrics {
    def withChildren(parent: String, children: List[String]): List[String] = {
      children.map { c =>
        s"$parent.$c"
      }
    }
    val columnNames: List[String] = {
      List(
        withChildren("word", PairwiseMetrics.columnNames),
        withChildren("line", PairwiseMetrics.columnNames),
        withChildren("context4th", ContextMetrics.columnNames),
        withChildren("context8th", ContextMetrics.columnNames),
        withChildren("context16th", ContextMetrics.columnNames),
        withChildren("closest4th", ContextIsClosest.columnNames),
        withChildren("closest8th", ContextIsClosest.columnNames),
        withChildren("closest16th",   ContextIsClosest.columnNames),
        List("lineIsClosestMatchInText")
      ).flatten
    }
    val numOfColumns = columnNames.size
  }
  final case class Metrics(phase1Metrics: Phase1Metrics,
                           lineIsClosestMatchInText: Boolean,
                           closest4th: ContextIsClosest,
                           closest8th: ContextIsClosest,
                           closest16th: ContextIsClosest) {

    def word: PairwiseMetrics = phase1Metrics.word
    def line: PairwiseMetrics = phase1Metrics.line
    def context4th: ContextMetrics = phase1Metrics.context4th
    def context8th: ContextMetrics = phase1Metrics.context8th
    def context16th: ContextMetrics = phase1Metrics.context16th
    def leftWord: Selection = phase1Metrics.leftWord
    def rightWord: Selection = phase1Metrics.rightWord
    def leftLineIdx: LineIdx = phase1Metrics.leftLineIdx
    def rightLineIdx: LineIdx = phase1Metrics.rightLineIdx

    @SuppressWarnings(Array(Warts.NonUnitStatement))
    def doubles: Array[Double]= {
      //I think System.arraycopy works equally well, just got sidetracked by other issues.
      //currently this is fast enough arraycopy might be faster but can't tell now
      val result = Array.ofDim[Double](Metrics.numOfColumns)
      val holder = new ArrayHolder(result)
      word.toDoubles(holder)
      line.toDoubles(holder)
      context4th.doubles(holder)
      context8th.doubles(holder)
      context16th.doubles(holder)
      closest4th.doubles(holder)
      closest8th.doubles(holder)
      closest16th.doubles(holder)
      holder.add(if (lineIsClosestMatchInText) 1.0 else 0.0)
      //todo this is not thread safe! (it's fine as long as the results are only read by one thread)
      result
    }

    override def toString: String = {
      s"$leftWord - $rightWord, word: $word" +
        s" 4: $context4th, 16: $context16th, lineIsClosest: $lineIsClosestMatchInText"
    }
  }

  def calcMetrics(left: String, right: String, supposedMaxLength: Int) = {
    //todo edit this so we can set maximum
    //todo http://www.mit.edu/~andoni/papers/compEdit.pdf
    //try n-grams
    //try prallelising
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
  private def calcAllMetrics(leftWord: WordWithContext, rightWord: WordWithContext, contextSize: Int, lineAlignmentCacher: LineAlignmentCacher) = {
    val leftWordString = leftWord.word.toText
    val rightWordString = rightWord.word.toText
    val wordMetrics = calcMetrics(leftWordString, rightWordString, math.max(leftWordString.length, rightWordString.length))

    val contextMetrics = if (wordMetrics.ldLenSim >= 0.99) {
      Some((
        calcShortenedContextMetrics(leftWord, rightWord, contextSize / 4),
        calcShortenedContextMetrics(leftWord, rightWord, contextSize / 8),
        calcShortenedContextMetrics(leftWord, rightWord, contextSize / 16)
      ))
    } else {
      None
    }
    contextMetrics.map { case (forth, eight, sixteenth) =>
      val leftSelection = leftWord.word
      val rightSelection = rightWord.word
      val lineMetrics = lineAlignmentCacher.calcLineMetrics(leftSelection.lineIdx, rightSelection.lineIdx)
      Phase1Metrics(leftSelection, rightSelection, word = wordMetrics, line = lineMetrics,
        context4th = forth, context8th = eight, context16th = sixteenth, leftLineIdx = leftSelection.lineIdx,
        rightLineIdx = rightSelection.lineIdx)
    }
  }

  /**
    * Position agnostic - as opposed to WordIndexRange which has position in it
    */
  final case class WordWithContextPositionAgnostic(before: String, after: String, word: String)
  final case class WordWithContext(beforeContext: String, afterContext: String, word: Selection) {
    def positionAgnostic: WordWithContextPositionAgnostic = WordWithContextPositionAgnostic(beforeContext, afterContext, word.toText)
    def shortedContext(len: Int): WordWithContext = {
      WordWithContext(beforeContext.takeRight(len), afterContext.take(len), word)
    }
  }

  private def findClosestLines(potentials: Map[_, Traversable[Phase1Metrics]]) = {
    potentials flatMap { case (_, oneGroup) =>
      //we know this will never throw because it doesn't make sense to have empty collection on the right side of the map
      @SuppressWarnings(Array(Warts.TraversableOps))
      val closest = oneGroup.minBy(_.line.normalizedLd)
      val closestNormalizedLd = closest.line.normalizedLd
      val allClosest = oneGroup.filter(p => math.abs(p.line.normalizedLd - closestNormalizedLd) < 0.0001)
      val ambiguous = allClosest.map(m => (m.leftLineIdx, m.rightLineIdx)).toSet.size > 1
      if (ambiguous) Traversable.empty
      else allClosest
    }
  }

  private def calcClosestLineMatches(phase1Metrics: Traversable[Phase1Metrics]) = {
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
    final case class ClosestContextMatches(beforeLeft: Traversable[Phase1Metrics], beforeRight: Traversable[Phase1Metrics],
                                           afterLeft: Traversable[Phase1Metrics], afterRight: Traversable[Phase1Metrics]) {
      //for performance, hashcode is much faster
      private def compress(metrics: Phase1Metrics) = {
        (metrics.leftWord, metrics.rightWord)
      }
      private val beforeLeftLookup = beforeLeft.map(compress).toSet
      private val beforeRightLookup = beforeRight.map(compress).toSet
      private val afterLeftLookup = afterLeft.map(compress).toSet
      private val afterRightLookup = afterRight.map(compress).toSet
      def in(phase1Metrics: Phase1Metrics): ContextIsClosest = {
        val metrics = compress(phase1Metrics)
        ContextIsClosest(beforeFromLeft = beforeLeftLookup.contains(metrics), beforeFromRight = beforeRightLookup.contains(metrics),
        afterFromLeft = afterLeftLookup.contains(metrics), afterFromRight = afterRightLookup.contains(metrics))
      }
    }
    private def findClosestContext(potentials: Map[_, Traversable[Phase1Metrics]], contextSelector: Phase1Metrics => Double) = {
      potentials flatMap { case (_, oneGroup) =>
        //we know this will never throw because it doesn't make sense to have empty collection on the right side of the map
        @SuppressWarnings(Array(Warts.TraversableOps))
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

    def calcClosestContextMatches(phase1Metrics: Traversable[Phase1Metrics], contextSelector: Phase1Metrics => ContextMetrics): ClosestContextMatches = {
      //So I found it too hard to do something I have done for line (resolving conflicts)
      //so I realised I could just not do that and do a feature based on both sides. It's simpler and potentially more info (or less, not sure)
      val leftWordPotentials = phase1Metrics.groupBy(_.leftWord)
      val rightWordPotentials = phase1Metrics.groupBy(_.rightWord)
      val (beforeLeft, beforeRight) = findClosestForSide(leftWordPotentials, rightWordPotentials, m => contextSelector(m).before)
      val (afterLeft, afterRight) = findClosestForSide(leftWordPotentials, rightWordPotentials, m => contextSelector(m).after)
      ClosestContextMatches(
        beforeLeft = beforeLeft, beforeRight = beforeRight,
        afterLeft = afterLeft, afterRight = afterRight
      )
    }

  }

  @tailrec
  private def findCandidates(contextSize: Int, lefts: Traversable[(WordWithContext, Candidacy)],
                             shortenedRights: Traversable[WordWithContext]): Traversable[(WordWithContext, Candidacy)] = {
    // 25 is because that's what we use inside too 100/4, should remove duplication
    if (contextSize < 25) lefts
    else {
      val candidateCtxFinder = new CandidateFinder(shortenedRights.toSet)

      val newLefts = lefts.map { case (word, candidates) =>
        word -> (candidates match {
          case exact: ExactMatches => exact
          case AllTheRest =>
            candidateCtxFinder.possibleMatches(word.shortedContext(contextSize))
        })
      }
      findCandidates(contextSize / 2, newLefts, shortenedRights.map(_.shortedContext(contextSize / 2)))
    }
  }

  def calcAlignerMetrics(left: FullText, right: FullText): Traversable[Metrics] = {
    val leftWords = Wordizer.toWordIndices(left.s).filter(_.length > 1)
    val rightWords = Wordizer.toWordIndices(right.s).filter(_.length > 1)

    logger.debug(s"leftWords: $leftWords")
    logger.debug(s"rightWords: $rightWords")
    logger.debug(s"Number of leftWords: ${leftWords.size}")
    logger.debug(s"Number of rightWords: ${rightWords.size}")
    val maxContextSize = 800
    val standardContextSize = 100
    val maxLeftContexts = leftWords.zipWithIndex map { case (word, leftIdx) =>
      val leftBeforeContext = context(leftIdx, leftWords, -maxContextSize)
      val leftAfterContext = context(leftIdx, leftWords, maxContextSize)
      WordWithContext(leftBeforeContext, leftAfterContext, word)
    }

    val maxRightContexts = rightWords.zipWithIndex map { case (word, rightIdx) =>
      val rightBeforeContext = context(rightIdx, rightWords, -maxContextSize)
      val rightAfterContext = context(rightIdx, rightWords, maxContextSize)
      WordWithContext(rightBeforeContext, rightAfterContext, word)
    }
    val standardRightContexts = maxRightContexts.map(_.shortedContext(standardContextSize))

    val standardRightContextsByWord = maxRightContexts.map(_.shortedContext(standardContextSize)).groupBy(_.word)

    //32 => 800/32 = 25 is because that's what we use inside too 100/4, should remove duplication
    val candidates = findCandidates(maxContextSize, maxLeftContexts.map(l => l -> AllTheRest), maxRightContexts)

    val lineAlignmentCacher = new LineAlignmentCacher(left.s.lines.map(_.trim).toVector, right.s.lines.map(_.trim).toVector)

    @SuppressWarnings(Array(Warts.TraversableOps))
    val phase1Metrics =
      candidates.flatMap { case (maxLeftWord, candidacy) =>
        // this is always max so this is fine
        val standardLeft = maxLeftWord.shortedContext(standardContextSize)
        candidacy match {
          case ExactMatches(matches) =>
            matches.flatMap { candidate =>
              val standardRight = standardRightContextsByWord.getOrElse(candidate.word, Traversable.empty)
              assert(standardRight.size ==== 1, s"${standardRight.size}")
              calcAllMetrics(standardLeft, standardRight.head, contextSize = standardContextSize, lineAlignmentCacher)
            }
          case AllTheRest =>
            standardRightContexts.flatMap { standardRight =>
              calcAllMetrics(standardLeft, standardRight, contextSize = standardContextSize, lineAlignmentCacher)
            }
        }
      }

    logger.debug(s"Metrics: ${phase1Metrics.mkString("\n")}")
    val closestLineMatches = calcClosestLineMatches(phase1Metrics)
    val closestContext4th = calcClosestContextMatches(phase1Metrics, _.context4th)
    val closestContext8th = calcClosestContextMatches(phase1Metrics, _.context8th)
    val closestContext16th = calcClosestContextMatches(phase1Metrics, _.context16th)
    phase1Metrics.map{m =>
      val closest = closestLineMatches.contains(m)
      Metrics(m, lineIsClosestMatchInText = closest, closest4th = closestContext4th.in(m),
        closest8th = closestContext8th.in(m), closest16th = closestContext16th.in(m))
    }
  }
}
