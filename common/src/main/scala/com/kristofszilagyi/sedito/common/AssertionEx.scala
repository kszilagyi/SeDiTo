package com.kristofszilagyi.sedito.common
import TypeSafeEqualsOps._
object AssertionEx {
  @SuppressWarnings(Array(Warts.Throw))
  def fail(msg: String): Nothing = throw new AssertionError(msg)

  @SuppressWarnings(Array(Warts.Null))
  def failIfNull(value: Any, msg: => Any): Unit = {
    assert(value !=== null, msg)
  }
}