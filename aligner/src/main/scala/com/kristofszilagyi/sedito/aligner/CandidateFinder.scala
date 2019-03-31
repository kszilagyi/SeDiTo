package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.aligner.Pass1FeatureCalculator.{WordWithContext, WordWithContextPositionAgnostic}
import com.kristofszilagyi.sedito.common.utils.MapOps.RichMap

sealed trait Candidacy
final case class ExactMatches(m: Traversable[WordWithContext]) extends Candidacy
object AllTheRest extends Candidacy
final class CandidateFinder(contexts: Set[WordWithContext]) {
  private val positionAgnosticWordWithContexts: Map[WordWithContextPositionAgnostic, Set[WordWithContext]] =
    contexts.map(c => c.positionAgnostic -> c).groupBy(_._1).mapValuesNow(_.map(_._2))

  def possibleMatches(s: WordWithContext): Candidacy = {
    val exactMatches = positionAgnosticWordWithContexts.getOrElse(s.positionAgnostic, Set.empty)
    if(exactMatches.nonEmpty) ExactMatches(exactMatches)
    else AllTheRest
  }
}
