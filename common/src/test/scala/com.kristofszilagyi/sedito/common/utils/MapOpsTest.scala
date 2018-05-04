package com.kristofszilagyi.sedito.common.utils

import com.kristofszilagyi.sedito.common.utils.MapOps.RichMap
import org.scalatest.FreeSpecLike
import org.scalatest.Matchers._

final class MapOpsTest extends FreeSpecLike{
  "empty" in {
    Map.empty[String, Int].mapValuesNow(_.toDouble) shouldBe Map.empty
  }

  "one" in {
    Map("hey" -> 1).mapValuesNow(_.toDouble) shouldBe Map("hey" -> 1.0)
  }

  "two" in {
    Map("hey" -> 1, "ho" -> 2).mapValuesNow(_.toDouble) shouldBe Map("hey" -> 1.0, "ho" -> 2.0)
  }
}
