package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.Wordizer
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class FixedCandidateFinderTest extends FreeSpecLike{
  private val words = Wordizer.toWordIndices("1234rs atomic sixers").toSet
  private val finder = new FixedCandidateFinder(words, substringSize = 2)

  "find one exact match" in {
    finder.possibleMatches("atomic").map(_.toWord) shouldBe Set("atomic")
  }

  "find one partial match" in {
    finder.possibleMatches("atarac").map(_.toWord) shouldBe Set("atomic")
  }

  "find multiple match" in {
    finder.possibleMatches("rssoon").map(_.toWord) shouldBe Set("sixers", "1234rs")
  }

  "find no match" in {
    finder.possibleMatches("randum").map(_.toWord) shouldBe Set.empty
  }

}
