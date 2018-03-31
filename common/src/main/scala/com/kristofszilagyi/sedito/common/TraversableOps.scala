package com.kristofszilagyi.sedito.common
import TypeSafeEqualsOps._
object TraversableOps {
  implicit class RichTraversable[A](traversable: Traversable[A]) {
    @SuppressWarnings(Array(Warts.TraversableOps))
    def single: Option[A] = {
      if (traversable.size ==== 1) Some(traversable.head)
      else None
    }
  }
}
