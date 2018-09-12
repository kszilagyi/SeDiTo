package com.kristofszilagyi.sedito.common

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.kristofszilagyi.sedito.common.TestCase._
import com.kristofszilagyi.sedito.common.Warts.discard
import org.log4s.getLogger
import spray.json.{JsonReader, enrichAny, enrichString}

import scala.io.Source
import scala.util.Try

final case class FullText(s: String)
object TestCase {
  private val logger = getLogger
  private val leftFileName = "left.txt"
  private val rightFileName = "right.txt"
  private val alignmentFileName = "alignment.json"
  def open(testDir: Path): Try[TestCase] = {
    logger.debug(s"Opening test case: $testDir")

    //todo handle line endings properly
    Try {

      val left = FullText(Source.fromFile(testDir.resolve(leftFileName).toFile).mkString)
      val right = FullText(Source.fromFile(testDir.resolve(rightFileName).toFile).mkString)
      implicit val jsonReader: JsonReader[AmbiguousWordAlignment] = AmbiguousWordAlignment.reader(Wordizer.calculateLines(left.s),
        Wordizer.calculateLines(right.s))
      val alignment = Source.fromFile(testDir.resolve(alignmentFileName).toFile).mkString.parseJson.convertTo[AmbiguousWordAlignment]
      (left, right, alignment)
    }.map{ case (left, right, alignment) =>
      val testCase = TestCase(left, right, alignment)
      logger.debug(s"TestCase opening successfully finished for $testDir")
      testCase
    }

  }


}

final case class TestCase(left: FullText, right: FullText, wordAlignment: AmbiguousWordAlignment) {
  def save(testDir: Path): Try[Unit] = {
    Try {
      discard(Files.write(testDir.resolve(leftFileName), left.s.getBytes(StandardCharsets.UTF_8)))
      discard(Files.write(testDir.resolve(rightFileName), right.s.getBytes(StandardCharsets.UTF_8)))
      val alignmentString = wordAlignment.toJson.sortedPrint
      discard(Files.write(testDir.resolve(alignmentFileName), alignmentString.getBytes(StandardCharsets.UTF_8)))
    }
  }
}
