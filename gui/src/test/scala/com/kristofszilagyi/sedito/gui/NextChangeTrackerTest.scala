package com.kristofszilagyi.sedito.gui

import com.kristofszilagyi.sedito.common.LineIdx
import org.scalatest.Matchers._
import org.scalatest.{Outcome, fixture}

class NextChangeTrackerTest extends fixture.FreeSpecLike {
  type FixtureParam = NextChangeTracker

  protected def withFixture(test: OneArgTest): Outcome = {
    test(createTracker())
  }

  private def eqPoint(leftStart: Int) = LineChangePoint.from((leftStart, leftStart + 1), (leftStart, leftStart + 1))
  private val first = eqPoint(1)
  private val middle = eqPoint(2)
  private val last = eqPoint(3)
  private def createTracker() = new NextChangeTracker(List(first, middle, last))

  "first" - {
    "starts with first (left)" in { tracker =>
      tracker.left() shouldBe first.left.from
    }

    "starts with first (right)" in { tracker =>
      tracker.right() shouldBe first.right.from
    }

    "moving back stays with first" in { tracker =>
      tracker.prev()
      tracker.left() shouldBe first.left.from
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
      tracker.left() shouldBe middle.left.from
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
      tracker.left() shouldBe first.left.from
    }
  }


  "last" - {
    "moving works" in { tracker =>
      tracker.next()
      tracker.next()
      tracker.left() shouldBe last.left.from
    }

    "moving too many times works" in { tracker =>
      tracker.next()
      tracker.next()
      tracker.next()
      tracker.left() shouldBe last.left.from
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
      tracker.left() shouldBe middle.left.from
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