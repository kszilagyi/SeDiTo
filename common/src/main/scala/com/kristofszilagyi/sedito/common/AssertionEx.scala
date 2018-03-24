package com.kristofszilagyi.sedito.common

object AssertionEx {
  @SuppressWarnings(Array(Warts.Throw))
  def fail(msg: String): Nothing = throw new AssertionError(msg)
}
