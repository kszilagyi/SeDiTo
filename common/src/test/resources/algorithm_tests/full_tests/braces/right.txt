package com.kristofszilagyi.sedito.common
import TypeSafeEqualsOps._
import com.kristofszilagyi.sedito.common.AssertionEx.fail
object TraversableOps {
  implicit class RichTraversable[A](traversable: Traversable[A]) {
    @SuppressWarnings(Array(Warts.TraversableOps))
    def single: Option[A] = {
      if (traversable.size ==== 1) Some(traversable.head)
      else None
    }

    def forceToOption: Option[A] = {
      if (traversable.size > 1) fail(s"Traversable's size is too big: $traversable")
      else {
        this.single
      }
    }
  }
}