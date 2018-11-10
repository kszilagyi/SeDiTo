package com.kristofszilagyi.sedito.gui

sealed trait SaveResult
case object Saved extends SaveResult
final case class SaveFailed(t: Throwable) extends SaveResult
case object NoPath extends SaveResult