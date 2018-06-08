package com.kristofszilagyi.sedito.common.utils

import spray.json.{JsNumber, JsString, JsValue, JsonFormat}

object JsonUtils {
  def wrappedString[T](from: String => T)(to: T => String): JsonFormat[T] = {
    new JsonFormat[T] {
      def read(json: JsValue): T = {
        json match {
          case JsString(s) => from(s)
          case other => spray.json.deserializationError(s"Expected string got $other")
        }
      }

      def write(obj: T): JsValue = JsString(to(obj))
    }
  }

  def wrappedInt[T](from: Int => T)(to: T => Int): JsonFormat[T] = {
    new JsonFormat[T] {
      def read(json: JsValue): T = {
        json match {
          case JsNumber(s) => from(s.toIntExact)
          case other => spray.json.deserializationError(s"Expected string got $other")
        }
      }

      def write(obj: T): JsValue = JsNumber(to(obj))
    }
  }
}
