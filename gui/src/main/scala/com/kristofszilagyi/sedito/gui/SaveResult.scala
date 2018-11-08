package com.kristofszilagyi.sedito.gui

trait SaveResult
case object Saved extends SaveResult
final case class LeftFailed(t: Throwable) extends SaveResult
final case class RightFailed(t: Throwable) extends SaveResult