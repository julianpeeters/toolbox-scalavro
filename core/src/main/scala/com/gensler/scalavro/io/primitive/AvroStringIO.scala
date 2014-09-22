package com.gensler.scalavro.io.primitive

import com.gensler.scalavro.types.primitive.AvroString
import com.gensler.scalavro.error.{ AvroSerializationException, AvroDeserializationException }

import org.apache.avro.generic.GenericData
import org.apache.avro.io.{ BinaryEncoder, BinaryDecoder }

import spray.json._

import scala.util.Try

object AvroStringIO extends AvroStringIO

trait AvroStringIO extends AvroNullablePrimitiveTypeIO[String] {

  val avroType = AvroString

  ////////////////////////////////////////////////////////////////////////////
  // BINARY ENCODING
  ////////////////////////////////////////////////////////////////////////////

  protected[scalavro] def write(
    value: String,
    encoder: BinaryEncoder): Unit = {
    if (value == null) {
      AvroLongIO.write(UNION_INDEX_NULL, encoder)
    }
    else {
      AvroLongIO.write(UNION_INDEX_VALUE, encoder)
      encoder writeString value
    }
  }
  def read(decoder: BinaryDecoder): String =
    AvroLongIO.read(decoder) match {
      case UNION_INDEX_NULL  => null
      case UNION_INDEX_VALUE => decoder.readString
    }

  ////////////////////////////////////////////////////////////////////////////
  // JSON ENCODING
  ////////////////////////////////////////////////////////////////////////////

  def writePrimitiveJson(value: String) = {
    if (value == null)
      JsNull
    else
      JsString(value)
  }

  def readJson(json: JsValue): Try[String] = Try {
    json match {
      case JsString(value) => value
      case JsNull          => null
      case _               => throw new AvroDeserializationException[String]
    }
  }

}
