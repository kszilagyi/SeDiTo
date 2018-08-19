package com.kristofszilagyi.sedito.common.utils

object TupleOps {
  implicit class RichTuple[T](t: (T, T)) {
    def sequence[U](implicit ev: T <:< Option[U]): Option[(U, U)] = {
      val maybeU1 = ev(t._1)
      val maybeU2 = ev(t._2)
      (maybeU1, maybeU2) match {
        case (Some(u1), Some(u2)) => Some((u1, u2))
        case _ => None
      }
    }
  }
}
