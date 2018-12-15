package com.kristofszilagyi.sedito.common.utils

import com.github.ghik.silencer.silent
import com.kristofszilagyi.sedito.common.Warts
import spray.json.{DefaultJsonProtocol, JsObject, JsValue, JsonReader, JsonWriter, RootJsonReader, RootJsonWriter}

//noinspection ScalaDeprecation
@silent //suppress deprecation warnings
@SuppressWarnings(Array(Warts.Overloading, Warts.NonUnitStatement))
object ExtraFormats extends DefaultJsonProtocol {

  def jsonWriter1[P1: JsonWriter, T <: Product :ClassManifest]: RootJsonWriter[T] = {
    val Array(p1) = extractFieldNames(classManifest[T])
    jsonWriter[P1, T](p1)
  }
  def jsonWriter[P1: JsonWriter, T <: Product](fieldName1: String): RootJsonWriter[T] = new RootJsonWriter[T]{
    def write(p: T): JsObject = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(1 * 2)
      fields ++= productElement2Field[P1](fieldName1, p, 0)
      JsObject(fields: _*)
    }
  }

  def jsonReader1[P1: JsonReader, T <: Product :ClassManifest](construct: P1 => T): RootJsonReader[T] = {
    val Array(p1) = extractFieldNames(classManifest[T])
    jsonReader1(construct, p1)
  }
  def jsonReader1[P1: JsonReader, T <: Product](construct: P1 => T, fieldName1: String): RootJsonReader[T] = new RootJsonReader[T]{
    def read(value: JsValue): T = {
      val p1V = fromField[P1](value, fieldName1)
      construct(p1V)
    }
  }

  def jsonWriter2[P1: JsonWriter, P2: JsonWriter, T <: Product :ClassManifest]: RootJsonWriter[T] = {
    val Array(p1, p2) = extractFieldNames(classManifest[T])
    jsonWriter[P1, P2, T](p1, p2)
  }
  def jsonWriter[P1: JsonWriter, P2: JsonWriter, T <: Product](fieldName1: String, fieldName2: String): RootJsonWriter[T] = new RootJsonWriter[T]{
    def write(p: T): JsObject = {
      val fields = new collection.mutable.ListBuffer[(String, JsValue)]
      fields.sizeHint(2 * 3)
      fields ++= productElement2Field[P1](fieldName1, p, 0)
      fields ++= productElement2Field[P2](fieldName2, p, 1)
      JsObject(fields: _*)
    }
  }
}
