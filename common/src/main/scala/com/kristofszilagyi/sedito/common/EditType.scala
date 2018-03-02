package com.kristofszilagyi.sedito.common



sealed trait EditType

case object Moved extends EditType
case object Inserted extends EditType
case object Deleted extends EditType
case object Same extends EditType


