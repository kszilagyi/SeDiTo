package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.LineIdx
import org.scalatest.Matchers._
import org.scalatest.{Outcome, fixture}

class NextChangeTrackerTest extends fixture.FreeSpecLike {
  type FixtureParam = NextChangeTracker

  protected def withFixture(test: OneArgTest): Outcome = {
    test(createTracker())
  }

  private def changePoint(leftStart: Int) = ChangePointStart(LineIdx(leftStart), LineIdx(leftStart))
  private val first = changePoint(1)
  private val middle = changePoint(2)
  private val last = changePoint(3)
  private def createTracker() = new NextChangeTracker(List(first, middle, last))

  "first" - {
    "starts with first (left)" in { tracker =>
      tracker.left() shouldBe first.left
    }

    "starts with first (right)" in { tracker =>
      tracker.right() shouldBe first.right
    }

    "moving back stays with first" in { tracker =>
      tracker.prev()
      tracker.left() shouldBe first.left
    }

    "can't go back" in { tracker =>
      tracker.hasPrev() shouldBe false
    }

    "can go ahead" in { tracker =>
      tracker.hasNext() shouldBe true
    }
  }

  "middle" - {
    "moving works" in { tracker =>
      tracker.next()
      tracker.left() shouldBe middle.left
    }

    "can go back" in { tracker =>
      tracker.next()
      tracker.hasPrev() shouldBe true
    }

    "can go ahead" in { tracker =>
      tracker.next()
      tracker.hasNext() shouldBe true
    }

    "moving back works" in { tracker =>
      tracker.next()
      tracker.prev()
      tracker.left() shouldBe first.left
    }
  }


  "last" - {
    "moving works" in { tracker =>
      tracker.next()
      tracker.next()
      tracker.left() shouldBe last.left
    }

    "moving too many times works" in { tracker =>
      tracker.next()
      tracker.next()
      tracker.next()
      tracker.left() shouldBe last.left
    }

    "can go back" in { tracker =>
      tracker.next()
      tracker.next()
      tracker.hasPrev() shouldBe true
    }

    "can't go ahead" in { tracker =>
      tracker.next()
      tracker.next()
      tracker.hasNext() shouldBe false
    }

    "moving back works" in { tracker =>
      tracker.next()
      tracker.next()
      tracker.prev()
      tracker.left() shouldBe middle.left
    }
  }

  "empty" - {
    "returns 0 for left" in { _ =>
      val tracker = new NextChangeTracker(List())
      tracker.left() shouldBe LineIdx(0)
    }

    "returns 0 for right" in { _ =>
      val tracker = new NextChangeTracker(List())
      tracker.right() shouldBe LineIdx(0)
    }
  }

}