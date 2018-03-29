package com.kristofszilagyi.sedito.common

import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._
final class LdLenSimilarityTest extends FreeSpecLike {
  "empty strings are not similar" in {
    LdLenSimilarity.calc("", "") shouldBe LdLenSimilarity(0)
  }

  "same 1 character long string has 1 similarity" in {
    LdLenSimilarity.calc("a", "a") shouldBe LdLenSimilarity(1)
  }

  "different 1 character long string has 0 similarity" in {
    LdLenSimilarity.calc("b", "a") shouldBe LdLenSimilarity(0)
  }


  "same 2 character long strings has bigger than 1 similarity" in {
    assert(LdLenSimilarity.calc("aa", "aa").d > 1)
  }

  "completely different 2 long strings has 0 similarity" in {
    LdLenSimilarity.calc("bb", "aa") shouldBe LdLenSimilarity(0)
  }

  "same 3 character long strings has bigger similarity than 2 character long sames" in {
    assert(LdLenSimilarity.calc("aaa", "aaa").d > LdLenSimilarity.calc("aa", "aa").d)
  }

  "3 character long strings with 1 edit distance have 1 similarity" in {
    LdLenSimilarity.calc("aaa", "baa").d shouldBe 1
  }

  "completely different 3 character long strings have 0 similarity" in {
    LdLenSimilarity.calc("aaa", "bbb").d shouldBe 0
  }


  "same 5 character long strings has bigger similarity than 3 character long sames" in {
    assert(LdLenSimilarity.calc("aaaaa", "aaaaa").d > LdLenSimilarity.calc("aaa", "aaa").d)
  }

  "5 character long strings with 2 edit distance have 1 similarity" in {
    LdLenSimilarity.calc("aaaaa", "bbaaa").d shouldBe 1
  }

  "completely different 5 character long strings have 0 similarity" in {
    LdLenSimilarity.calc("aaaaa", "bbbbb").d shouldBe 0
  }

  "comparing different lengths strings behave as expected (same subtext)" in {
    LdLenSimilarity.calc("aaa", "aaaaa").d shouldBe 1
  }

  "comparing different lengths strings behave as expected (0 0 similarity)" in {
    LdLenSimilarity.calc("bbb", "aaaaa").d shouldBe 0
  }

}
