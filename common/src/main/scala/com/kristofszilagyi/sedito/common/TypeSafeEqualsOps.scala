package com.kristofszilagyi.sedito.common

object TypeSafeEqualsOps {
  @SuppressWarnings(Array(Warts.Equals))
  implicit final class AnyOps[A](self: A) {
    def ====(other: A): Boolean = self == other
    def !===(other: A): Boolean = self != other
  }
}
