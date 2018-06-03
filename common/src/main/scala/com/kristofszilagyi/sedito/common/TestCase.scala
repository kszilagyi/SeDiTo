package com.kristofszilagyi.sedito.common

import java.nio.file.Path

import org.log4s.getLogger
import spray.json.enrichString

import scala.io.Source
import scala.util.Try

object TestCase {
  private val logger = getLogger

  def open(testDir: Path): Try[TestCase] = {
    logger.info(s"Opening test case: $testDir")

    //todo handle line endings properly
    Try {

      val left = Source.fromFile(testDir.resolve("left.txt").toFile).mkString
      val right = Source.fromFile(testDir.resolve("right.txt").toFile).mkString
      val alignment = Source.fromFile(testDir.resolve("alignment.json").toFile).mkString.parseJson.convertTo[AmbiguousLineAlignment]
      (left, right, alignment)
    }.map{ case (left, right, alignment) =>
      val testCase = TestCase(left, right, AmbiguousWordAlignment.fromOld(Lines.from(left), Lines.from(right), alignment))
      logger.info(s"TestCase opening successfully finished for $testDir")
      testCase
    }

  }
}

final case class TestCase(left: String, right: String, wordAlignment: AmbiguousWordAlignment)
