package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.MetricCalculator.{WordWithContext, WordWithContextPositionAgnostic}
import com.kristofszilagyi.sedito.common.utils.MapOps.RichMap

final class CandidateFinder(contexts: Set[WordWithContext]) {
  private val positionAgnosticWordWithContexts: Map[WordWithContextPositionAgnostic, Set[WordWithContext]] =
    contexts.map(c => c.positionAgnostic -> c).groupBy(_._1).mapValuesNow(_.map(_._2))

  def possibleMatches(s: WordWithContext): Set[WordWithContext] = {
    val exactMatches = positionAgnosticWordWithContexts.getOrElse(s.positionAgnostic, Set.empty)
    if(exactMatches.nonEmpty) exactMatches
    else contexts
  }
}
