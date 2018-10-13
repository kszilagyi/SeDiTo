package com.kristofszilagyi.sedito.common

import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class WordizerTest extends FreeSpecLike {
  "empty is empty" in {
    Wordizer.toWords("") shouldBe Seq.empty
  }

  "1 word" in {
    Wordizer.toWords("apple") shouldBe Seq("apple")
  }

  "1 word _" in {
    Wordizer.toWords("apple_juice") shouldBe Seq("apple_juice")
  }

  "2 words, 1 space" in {
    Wordizer.toWords("apple cheese") shouldBe Seq("apple", "cheese")
  }

  "2 words, 2 space" in {
    Wordizer.toWords("apple  cheese") shouldBe Seq("apple", "cheese")
  }

  "starting with space" in {
    Wordizer.toWords(" apple cheese") shouldBe Seq("apple", "cheese")
  }

  "2 words, 1 comma" in {
    Wordizer.toWords("apple,cheese") shouldBe Seq("apple", ",", "cheese")
  }

  "complicated scenario" in {
    Wordizer.toWords(", ?apple; ,   cheese_to ad ; baba") shouldBe Seq(",", "?", "apple", ";", ",", "cheese_to", "ad", ";", "baba")
  }

  "splits on dash" in {
    Wordizer.toWords("apple-cheese") shouldBe Seq("apple", "-", "cheese")
  }

  //with whitespaces tests
  "2 words, 1 space - keep whitespaces" in {
    Wordizer.toWordsAndWhiteSpaces("apple cheese") shouldBe Seq("apple", " ", "cheese")
  }

  "2 words, 2 space - keep whitespaces" in {
    Wordizer.toWordsAndWhiteSpaces("apple  cheese") shouldBe Seq("apple", " ", " ", "cheese")
  }

  "starting with space - keep whitespaces" in {
    Wordizer.toWordsAndWhiteSpaces(" apple cheese") shouldBe Seq(" ", "apple"," ", "cheese")
  }
}
