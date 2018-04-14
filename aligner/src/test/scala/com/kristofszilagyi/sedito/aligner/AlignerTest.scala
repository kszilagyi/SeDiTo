package com.kristofszilagyi.sedito.aligner

import com.kristofszilagyi.sedito.common.Wordizer
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class AlignerTest extends FreeSpecLike{

  private val line = "How do you do, my darling? Have you had breakfast yet?"
  private val words = Wordizer.toWordIndices(line)


  "empty context" in {
    Aligner.context(7, words, 0) shouldBe ""
  }

  "negative context works (word edge)" in {
    Aligner.context(6, words, -5) shouldBe "do,my"
  }

  "negative context works (not word edge)" in {
    Aligner.context(6, words, -6) shouldBe "youdo,my"
  }

  "positive context works (word edge)" in {
    Aligner.context(6, words, 5) shouldBe "?Have"
  }

  "positive context works (not word edge)" in {
    Aligner.context(6, words, 6) shouldBe "?Haveyou"
  }
}
