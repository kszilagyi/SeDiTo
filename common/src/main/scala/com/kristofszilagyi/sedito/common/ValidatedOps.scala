package com.kristofszilagyi.sedito.common

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.kristofszilagyi.sedito.common.AssertionEx.fail

object ValidatedOps {
  @SuppressWarnings(Array(Warts.Overloading))
  implicit class RichValidated[E, A](v: Validated[E, A]) {
    def getAssert(msg: String): A = {
      v match {
        case Valid(a) => a
        case Invalid(e) => fail(s"Bug in code - $msg: $e")
      }
    }

    def getAssert: A = {
      v match {
        case Valid(a) => a
        case Invalid(e) => fail(s"Bug in code - $e")
      }
    }
  }
}
