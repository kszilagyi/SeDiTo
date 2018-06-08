package com.kristofszilagyi.sedito.gui

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import com.kristofszilagyi.sedito.common.{TestCase, Warts}
import com.kristofszilagyi.sedito.common.utils.Control.using
import org.scalatest.FreeSpecLike
import spray.json.enrichAny

import scala.collection.JavaConverters._
import scala.util.{Failure, Success}

class ConvertToNew extends FreeSpecLike{
  //throwaway tool

  @SuppressWarnings(Array(Warts.Throw))
  private def readTestCase(testDir: Path): TestCase = {
    TestCase.open(testDir) match {
      case Failure(exception) =>
        throw exception
      case Success(testCase) => testCase
    }
  }
  "convert" in {
    val parentDir = Paths.get("common/src/test/resources/algorithm_tests/full_tests")
    println(parentDir)
    val testDirs = using(Files.newDirectoryStream(parentDir)) { stream =>
      stream.iterator().asScala.toList.filter(p => Files.isDirectory(p))
    }
    val testCases = testDirs.map(td => td -> readTestCase(td))
    testCases.foreach { case (testDir, tc) =>
      val alignmentString = tc.wordAlignment.toJson.prettyPrint
      Files.write(testDir.resolve("alignment.json"), alignmentString.getBytes(StandardCharsets.UTF_8))
    }
  }

}
