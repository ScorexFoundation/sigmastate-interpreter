package sigmastate.utils

import sigmastate.SType
import sigmastate.Values.Value
import Extensions._
import sigmastate.serialization.TypeSerializer

trait ByteWriter {
  def put(x: Byte): ByteWriter
  def putBoolean(x: Boolean): ByteWriter
  def putShort(x: Short): ByteWriter
  def putInt(x: Int): ByteWriter
  def putLong(x: Long): ByteWriter
  def putBytes(xs: Array[Byte]): ByteWriter
  def putOption[T](x: Option[T])(putValue: (ByteWriter, T) => Unit): ByteWriter
  def putType[T <: SType](x: T): ByteWriter
  def putValue[T <: SType](x: Value[T]): ByteWriter
  def toBytes: Array[Byte]
}

class ByteArrayWriter(b: ByteArrayBuilder) extends ByteWriter {
  @inline def put(x: Byte): ByteWriter = { b.append(x); this }
  @inline def putBoolean(x: Boolean): ByteWriter = { b.append(x); this }
  @inline def putShort(x: Short): ByteWriter = { b.append(x); this }
  @inline def putInt(x: Int): ByteWriter = { b.append(x); this }
  @inline def putLong(x: Long): ByteWriter = { b.append(x); this }
  @inline def putBytes(xs: Array[Byte]): ByteWriter = { b.append(xs); this }
  @inline def putOption[T](x: Option[T])(putValue: (ByteWriter, T) => Unit): ByteWriter = { b.appendOption(x)(v => putValue(this, v)); this }
  @inline def putType[T <: SType](x: T): ByteWriter = { TypeSerializer.serialize(x, this); this }
  @inline def putValue[T <: SType](x: Value[T]): ByteWriter = { b.appendValue(x); this }
  @inline def toBytes: Array[Byte] = b.toBytes
}