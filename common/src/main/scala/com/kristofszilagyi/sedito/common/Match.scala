package com.kristofszilagyi.sedito.common

import spray.json.DefaultJsonProtocol._
import spray.json.JsonFormat

object Match {
  implicit val format: JsonFormat[Match] = jsonFormat2(Match.apply)
}
final case class Match(leftLineIdx: Int, rightLineIdx: Int)


object Alignment {
  implicit val format: JsonFormat[Alignment] = jsonFormat1(Alignment.apply)
}
final case class Alignment(matches: Set[Match])
