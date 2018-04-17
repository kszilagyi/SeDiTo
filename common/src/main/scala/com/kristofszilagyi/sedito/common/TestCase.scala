package com.kristofszilagyi.sedito.common

import java.nio.file.Path

import spray.json.enrichString

import scala.io.Source
import scala.util.Try

object TestCase {
  def open(testDir: Path): Try[TestCase] = {
    //todo handle line endings properly
    Try {

      val left = Source.fromFile(testDir.resolve("left.txt").toFile).mkString
      val right = Source.fromFile(testDir.resolve("right.txt").toFile).mkString
      val alignment = Source.fromFile(testDir.resolve("alignment.json").toFile).mkString.parseJson.convertTo[UnambiguousLineAlignment]
      (left, right, alignment)
    }.map{ case (left, right, alignment) =>
      TestCase(left, right, WordAlignment.fromOld(Lines.from(left), Lines.from(right), alignment))
    }
  }
}

final case class TestCase(left: String, right: String, wordAlignment: WordAlignment)
