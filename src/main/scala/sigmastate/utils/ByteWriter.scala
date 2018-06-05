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

  @inline def putULong(x: Long): ByteWriter = {
    // todo: ensure capacity in ByteArrayBuilder
    // todo: is it too ugly already?
    val buffer = b.array()
    var position = b.length()
    var value = x
    // should be fast if java -> scala conversion did not botched it
    // borrowed from http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L1387
    while (true) {
      if ((value & ~0x7FL) == 0) {
        buffer(position) = value.asInstanceOf[Byte]
        position += 1
        b.setLength(position)
        return this
      }
      else {
        buffer(position) = ((value.asInstanceOf[Int] & 0x7F) | 0x80).toByte
        position += 1
        value >>>= 7
      }
    }
    this
  }

  @inline def putBytes(xs: Array[Byte]): ByteWriter = { b.append(xs); this }
  @inline def putOption[T](x: Option[T])(putValue: (ByteWriter, T) => Unit): ByteWriter = { b.appendOption(x)(v => putValue(this, v)); this }
  @inline def putType[T <: SType](x: T): ByteWriter = { TypeSerializer.serialize(x, this); this }
  @inline def putValue[T <: SType](x: Value[T]): ByteWriter = { b.appendValue(x); this }
  @inline def toBytes: Array[Byte] = b.toBytes
}
