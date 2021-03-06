package com.kristofszilagyi.sedito.common

import com.kristofszilagyi.sedito.common.TypeSafeEqualsOps._

import scala.annotation.tailrec


object LineAligner {

  private final case class Ld(left: LineIdx, right: LineIdx, similarity: Double)

  @tailrec
  private def approximateBestMatching(lds: Seq[Ld], result: Set[LineMatch]): Set[LineMatch] = {
    lds.toList match {
      case best :: rest =>
        val newResult = result + LineMatch(best.left, best.right)
        val restWithoutConflict = rest.filterNot{r =>
          r.left ==== best.left || r.right ==== best.right
        }
        approximateBestMatching(restWithoutConflict, newResult)
      case Nil =>
        result
    }
  }
  def align(alignment: UnambiguousWordAlignment): UnambiguousLineAlignment = {
    val lineMatches = alignment.matches.toList.groupBy(m => (m.left.lineIdx, m.right.lineIdx))
    val linewiseSimilarity = lineMatches.map { case ((leftLine, rightLine), matches) =>
      val similarities = matches.map { m =>
        val left = m.left.toText
        val right = m.right.toText
        LdLenSimilarity.calc(left, right)
      }
      Ld(leftLine, rightLine, similarities.sum)
    }
    //no filtering as for display it's better to display then related even if only a little bit than non-related at all
    val sortedSimilarity = linewiseSimilarity.toSeq.sortBy(- _.similarity) //the higher similarity is better so reverse sort
    UnambiguousLineAlignment(approximateBestMatching(sortedSimilarity, result = Set.empty))
  }
}
