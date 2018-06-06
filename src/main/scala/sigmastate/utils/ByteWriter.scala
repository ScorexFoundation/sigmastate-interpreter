package sigmastate.utils

import java.util._

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.TypeSerializer
import sigmastate.utils.ByteArrayWriter.{encodeZigZagInt, encodeZigZagLong}
import sigmastate.utils.Extensions._

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
  @inline override def put(x: Byte): ByteWriter = { b.append(x); this }
  @inline override def putBoolean(x: Boolean): ByteWriter = { b.append(x); this }
  @inline override def putShort(x: Short): ByteWriter = { b.append(x); this }
  @inline override def putInt(x: Int): ByteWriter = { b.append(x); this }

  /**
    * Encode signed Int using VLQ.
    *
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @note The resulting varint uses ZigZag encoding, which is much more efficient.
    *       Have to be decoded '''only''' with [[ByteBufferReader.getSInt]]
    * @param x signed Int
    */
  @inline def putSInt(x: Int): ByteWriter = putULong(encodeZigZagInt(x))

  /**
    * Encode unsigned Int using VLQ.
    *
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @note Don't use it as the type for a negative number, the resulting varint is always six
    *       bytes long – it is, effectively, treated like a very large unsigned integer.
    *       If you use [[putSInt]], the resulting varint uses ZigZag encoding,
    *       which is much more efficient.
    * @param x preferably unsigned Int (signed value will produce a significant overhead,
    *          see note above)
    */
  @inline def putUInt(x: Int): ByteWriter = putULong(x.toLong)

  @inline override def putLong(x: Long): ByteWriter = { b.append(x); this }

  /**
    * Encode signed Long using VLQ.
    *
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @note The resulting varint uses ZigZag encoding, which is much more efficient.
    *       Have to be decoded '''only''' with [[ByteBufferReader.getSLong]]
    * @param x signed Long
    */
  @inline def putSLong(x: Long): ByteWriter = putULong(encodeZigZagLong(x))

  /**
    * Encode unsigned Long using VLQ.
    *
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @note Don't use it as the type for a negative number, the resulting varint is always ten
    *       bytes long – it is, effectively, treated like a very large unsigned integer.
    *       If you use [[putSLong]], the resulting varint uses ZigZag encoding,
    *       which is much more efficient.
    * @param x preferably unsigned Long (signed value will produce a significant overhead,
    *          see note above)
    */
  @inline def putULong(x: Long): ByteWriter = {
    val buffer = new Array[Byte](10)
    var position = 0
    var value = x
    // should be fast if java -> scala conversion did not botched it
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L1387
    while (true) {
      if ((value & ~0x7FL) == 0) {
        buffer(position) = value.asInstanceOf[Byte]
        position += 1
        b.append(Arrays.copyOf(buffer, position))
        return this
      } else {
        buffer(position) = ((value.asInstanceOf[Int] & 0x7F) | 0x80).toByte
        position += 1
        value >>>= 7
      }
    }
    this
    // see https://rosettacode.org/wiki/Variable-length_quantity for implementations in other languages
  }

  @inline override def putBytes(xs: Array[Byte]): ByteWriter = { b.append(xs); this }
  @inline override def putOption[T](x: Option[T])(putValue: (ByteWriter, T) => Unit): ByteWriter = { b.appendOption(x)(v => putValue(this, v)); this }
  @inline override def putType[T <: SType](x: T): ByteWriter = { TypeSerializer.serialize(x, this); this }
  @inline override def putValue[T <: SType](x: Value[T]): ByteWriter = { b.appendValue(x); this }
  @inline override def toBytes: Array[Byte] = b.toBytes
}

object ByteArrayWriter {

  /**
    * Encode a ZigZag-encoded 32-bit value.  ZigZag encodes signed integers
    * into values that can be efficiently encoded with varint.  (Otherwise,
    * negative values must be sign-extended to 64 bits to be varint encoded,
    * thus always taking 10 bytes on the wire.)
    *
    * @see [[https://developers.google.com/protocol-buffers/docs/encoding#types]]
    *
    * @param n signed Int
    * @return unsigned Int stored in a signed Int
    */
  def encodeZigZagInt(n: Int): Int =
  // Note:  the right-shift must be arithmetic
  // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L934
    (n << 1) ^ (n >> 31)

  /**
    * Encode a ZigZag-encoded 64-bit value.  ZigZag encodes signed integers
    * into values that can be efficiently encoded with varint.  (Otherwise,
    * negative values must be sign-extended to 64 bits to be varint encoded,
    * thus always taking 10 bytes on the wire.)
    *
    * @see [[https://developers.google.com/protocol-buffers/docs/encoding#types]]
    * @param n signed Long
    * @return unsigned Long stored in a signed Long
    */
  def encodeZigZagLong(n: Long): Long =
  // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L949
  // Note:  the right-shift must be arithmetic
    (n << 1) ^ (n >> 63)

}
