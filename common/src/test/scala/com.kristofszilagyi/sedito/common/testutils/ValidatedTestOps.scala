package com.kristofszilagyi.sedito.common.testutils

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.kristofszilagyi.sedito.common.AssertionEx._

object ValidatedTestOps {
  implicit class RichValidated[E, A](v: Validated[E, A]) {
    def get: A = {
      v match {
        case Valid(a) => a
        case Invalid(e) =>
          fail(s"Bug in test: $e")
      }
    }
  }
}
