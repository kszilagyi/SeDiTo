package com.kristofszilagyi.sedito.common

import java.nio.file.Paths

import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._
import spray.json.{JsonReader, enrichAny}

final class AmbiguousWordAlignmentJsonTest extends FreeSpecLike {
  "rountrip works" in {
    val rootPath = Paths.get(getClass.getClassLoader.getResource("algorithm_tests/full_tests").getPath)
    val path = rootPath.resolve("17_complex")
    @SuppressWarnings(Array(Warts.TryPartial))
    val testCase = TestCase.open(path).get
    implicit val reader: JsonReader[AmbiguousWordAlignment] =
      AmbiguousWordAlignment.reader(Wordizer.calculateLines(testCase.left.s), Wordizer.calculateLines(testCase.right.s))
    testCase.wordAlignment.toJson.convertTo[AmbiguousWordAlignment] shouldBe testCase.wordAlignment

  }

}
