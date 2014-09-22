package com.gensler.scalavro.io

import com.gensler.scalavro.error._
import com.gensler.scalavro.types.{ AvroType, AvroPrimitiveType }
import com.gensler.scalavro.util.Logging

import org.apache.avro.io.{
  EncoderFactory,
  DecoderFactory,
  BinaryEncoder,
  BinaryDecoder
}

import scala.collection.mutable
import scala.util.{ Try, Success, Failure }
import scala.reflect.runtime.universe.TypeTag

import spray.json._

import java.io.{ InputStream, OutputStream }

abstract class AvroTypeIO[T: TypeTag] extends Logging {

  /**
    * Returns the corresponding AvroType to this AvroTypeIO wrapper.
    */
  def avroType: AvroType[T]

  ////////////////////////////////////////////////////////////////////////////
  // BINARY ENCODING
  ////////////////////////////////////////////////////////////////////////////

  /**
    * Writes a serialized representation of the supplied object according to
    * the Avro specification for binary encoding.  Throws an
    * AvroSerializationException if writing is unsuccessful.
    *
    * Output buffering is dependent upon the supplied `OutputStream`.
    *
    * The caller is responsible for calling `flush`; this method
    * may not flush the target stream.
    */
  @throws[AvroSerializationException[_]]
  final def write[G <: T: TypeTag](obj: G, stream: OutputStream): Unit = {
    val encoder = EncoderFactory.get.directBinaryEncoder(stream, null)
    write(obj, encoder, mutable.Map[Any, Long](), true)
  }

  /**
    * Writes a serialized representation of the supplied object according to
    * the Avro specification for binary encoding.  Throws an
    * AvroSerializationException if writing is unsuccessful.
    *
    * Output buffering is dependent upon the supplied `BinaryEncoder`.
    *
    * The caller is responsible for calling `flush`; this method
    * may not flush the target stream.
    */
  @throws[AvroSerializationException[_]]
  protected[scalavro] def write[G <: T: TypeTag](
    obj: G,
    encoder: BinaryEncoder,
    references: mutable.Map[Any, Long],
    topLevel: Boolean): Unit

  /**
    * Attempts to create an object of type T by reading the required data from
    * the supplied binary stream.
    */
  @throws[AvroDeserializationException[_]]
  def read(stream: InputStream): Try[T] = Try {
    val decoder = DecoderFactory.get.directBinaryDecoder(stream, null)
    read(decoder, mutable.ArrayBuffer[Any](), true)
  }

  /**
    * Attempts to create an object of type T by reading the required data from
    * the supplied decoder.
    */
  @throws[AvroDeserializationException[_]]
  protected[scalavro] def read(
    decoder: BinaryDecoder,
    references: mutable.ArrayBuffer[Any],
    topLevel: Boolean): T

  ////////////////////////////////////////////////////////////////////////////
  // JSON ENCODING
  ////////////////////////////////////////////////////////////////////////////

  /**
    * Returns a serialized representation of the supplied object according to
    * the Avro specification for JSON encoding.  Throws an
    * AvroSerializationException if writing is unsuccessful.
    */
  @throws[AvroSerializationException[_]]
  def writeJson[G <: T: TypeTag](obj: G): JsValue

  /**
    * Returns a serialized representation of the supplied object according to
    * the Avro specification for JSON encoding.  Throws an
    * AvroSerializationException if writing is unsuccessful.
    */
  // @throws[AvroSerializationException[_]]
  // def writeJson[G <: T: TypeTag](tb: scala.tools.reflect.ToolBox[reflect.runtime.universe.type], obj: G): JsValue

  /**
    * Attempts to create an object of type T by reading the required data from
    * the supplied JsValue.
    */
  @throws[AvroDeserializationException[_]]
  def readJson(json: JsValue): Try[T]

  /**
    * Attempts to create an object of type T by reading the required data from
    * the supplied JSON String.
    */
  @throws[AvroDeserializationException[_]]
  final def readJson(jsonString: String): Try[T] = Try {
    readJson(jsonString.parseJson).get
  }

}

/**
  * Companion object for [[AvroTypeIO]]
  *
  * Contains conversions from any AvroType to a corresponding
  * AvroTypeIO capable of reading and writing.
  */
object AvroTypeIO {

  import scala.language.implicitConversions

  import com.gensler.scalavro.types.primitive._
  import com.gensler.scalavro.io.primitive._
  import com.gensler.scalavro.util.Union
  import com.gensler.scalavro.util.FixedData

  import com.gensler.scalavro.types.complex._
  import com.gensler.scalavro.io.complex._

  // primitive types
  def avroTypeToIO[T](avroType: AvroPrimitiveType[T]): AvroTypeIO[T] =
    avroType match {
      case AvroBoolean       => AvroBooleanIO
      case AvroBytes         => AvroBytesIO
      case AvroDouble        => AvroDoubleIO
      case AvroFloat         => AvroFloatIO
      case AvroByte          => AvroByteIO
      case AvroChar          => AvroCharIO
      case AvroShort         => AvroShortIO
      case AvroInt           => AvroIntIO
      case AvroLong          => AvroLongIO
      case AvroNull          => AvroNullIO
      case AvroString        => AvroStringIO // Covers Scala & Java strings (same class internally)
      case AvroXml           => AvroXmlIO

      case AvroJavaBoolean   => AvroJavaBooleanIO
      //case AvroJavaBytes     => AvroJavaBytesIO
      case AvroJavaDouble    => AvroJavaDoubleIO
      case AvroJavaFloat     => AvroJavaFloatIO
      case AvroJavaByte      => AvroJavaByteIO
      case AvroJavaCharacter => AvroJavaCharacterIO
      case AvroJavaShort     => AvroJavaShortIO
      case AvroJavaInteger   => AvroJavaIntegerIO
      case AvroJavaLong      => AvroJavaLongIO
      //case AvroW3Xml         => AvroW3XmlIO
    }

  // complex types
  def avroTypeToIO[T, S <: Seq[T]](array: AvroArray[T, S]): AvroArrayIO[T, S] = AvroArrayIO(array)
  def avroTypeToIO[T](array: AvroJArray[T]): AvroJArrayIO[T] = AvroJArrayIO(array)
  def avroTypeToIO[T, S <: Set[T]](set: AvroSet[T, S]): AvroSetIO[T, S] = AvroSetIO(set)
  def avroTypeToIO[T <: Enumeration](enum: AvroEnum[T]): AvroEnumIO[T] = AvroEnumIO(enum)
  def avroTypeToIO[T](enum: AvroJEnum[T]): AvroJEnumIO[T] = AvroJEnumIO(enum)
  def avroTypeToIO[T <: FixedData](fixed: AvroFixed[T]): AvroFixedIO[T] = AvroFixedIO(fixed)(fixed.tag)
  def avroTypeToIO[T, M <: Map[String, T]](map: AvroMap[T, M]): AvroMapIO[T, M] = AvroMapIO(map)
  def avroTypeToIO[T](error: AvroError[T]): AvroRecordIO[T] = AvroRecordIO(error)
  def avroTypeToIO[T](record: AvroRecord[T]): AvroRecordIO[T] = AvroRecordIO(record)
  def avroTypeToIO[T](tb: scala.tools.reflect.ToolBox[reflect.runtime.universe.type], record: AvroRecord[T]): ToolBoxAvroRecordIO[T] = ToolBoxAvroRecordIO(tb, record)
  def avroTypeToIO[U <: Union.not[_], T](union: AvroUnion[U, T]): AvroUnionIO[U, T] = AvroUnionIO(union)(union.union.underlyingTag, union.tag)

  def avroTypeToIO[T: TypeTag](at: AvroType[T]): AvroTypeIO[T] = {
    at match {
      case t: AvroPrimitiveType[_] => avroTypeToIO(t)
      case t: AvroArray[_, _]      => avroTypeToIO(t)
      case t: AvroJArray[_]        => avroTypeToIO(t)
      case t: AvroSet[_, _]        => avroTypeToIO(t)
      case t: AvroEnum[_]          => avroTypeToIO(t)
      case t: AvroJEnum[_]         => avroTypeToIO(t)
      case t: AvroFixed[_]         => avroTypeToIO(t)
      case t: AvroMap[_, _]        => avroTypeToIO(t)
      case t: AvroError[_]         => avroTypeToIO(t)
      case t: AvroRecord[_]        => avroTypeToIO(t)
      case t: AvroUnion[_, _]      => avroTypeToIO(t)
    }
  }

  def avroTypeToIO[T: TypeTag](tb: scala.tools.reflect.ToolBox[reflect.runtime.universe.type], at: AvroType[T]): AvroTypeIO[T] = {
    at match {
      case t: AvroPrimitiveType[_] => avroTypeToIO(t)
      case t: AvroArray[_, _]      => avroTypeToIO(t)
      case t: AvroJArray[_]        => avroTypeToIO(t)
      case t: AvroSet[_, _]        => avroTypeToIO(t)
      case t: AvroEnum[_]          => avroTypeToIO(t)
      case t: AvroJEnum[_]         => avroTypeToIO(t)
      case t: AvroFixed[_]         => avroTypeToIO(t)
      case t: AvroMap[_, _]        => avroTypeToIO(t)
      case t: AvroError[_]         => avroTypeToIO(t)
      case t: AvroRecord[_]        => avroTypeToIO(tb, t)
      case t: AvroUnion[_, _]      => avroTypeToIO(t)
    }
  }.asInstanceOf[AvroTypeIO[T]]

}
