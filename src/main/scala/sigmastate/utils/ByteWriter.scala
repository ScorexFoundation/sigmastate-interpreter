package sigmastate.utils

import java.util._

import sigmastate.utils.ByteArrayWriter.{encodeZigZagInt, encodeZigZagLong}
import sigmastate.utils.Extensions._

trait ByteWriter {
  def put(x: Byte): this.type

  /** Encode integer as an unsigned byte asserting the range check
    * @param x integer value to encode
    * @return
    * @throws AssertionError if x is outside of the unsigned byte range
    */
  def putUByte(x: Int): this.type = {
    assert(x >= 0 && x <= 0xFF, s"$x is out of unsigned byte range")
    put(x.toByte)
  }
  def putBoolean(x: Boolean): this.type
  def putShort(x: Short): this.type

  /**
    * Encode Short that are positive
    *
    * Use [[putShort]] to encode values that might be negative.
    * @param x Short
    */
  def putUShort(x: Int): this.type

  /**
    * Encode signed Int.
    * Use [[putUInt]] to encode values that are positive.
    *
    * @param x Int
    */
  def putInt(x: Int): this.type

  /**
    * Encode Int that are positive.
    * Use [[putInt]] to encode values that might be negative.
    *
    * @param x Int
    */
  def putUInt(x: Long): this.type

  /**
    * Encode signed Long.
    * Use [[putULong]] to encode values that are positive.
    *
    * @param x Long
    */
  def putLong(x: Long): this.type

  /**
    * Encode Long that are positive.
    * Use [[putLong]] to encode values that might be negative.
    *
    * @param x Long
    */
  def putULong(x: Long): this.type

  def putBytes(xs: Array[Byte]): this.type

  /**
    * Encode an array of boolean values as a bit array
    *
    * @param xs array of boolean values
    */
  def putBits(xs: Array[Boolean]): this.type
  def putOption[T](x: Option[T])(putValue: (this.type, T) => Unit): this.type
  def toBytes: Array[Byte]
}

/**
  * Not thread safe
  */
class ByteArrayWriter(b: ByteArrayBuilder) extends ByteWriter {
  @inline override def put(x: Byte): this.type = { b.append(x); this }
  @inline override def putBoolean(x: Boolean): this.type = { b.append(x); this }
  @inline override def putShort(x: Short): this.type = { b.append(x); this }

  /**
    * Encode unsigned Short value using VLQ.
    * Only positive values are supported, Use [[putShort]]
    * to encode negative and positive values.
    *
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @param x unsigned Short
    * @throws AssertionError for values not in unsigned Short range
    */
  @inline override def putUShort(x: Int): this.type = {
    assert(x >= 0 && x <= 0xFFFF, s"$x is out of unsigned short range")
    putUInt(x)
  }


  /**
    * Encode signed Int using VLQ with ZigZag.
    * Both negative and positive values are supported, but due to ZigZag encoding positive
    * values is done less efficiently than by [[putUInt]].
    * Use [[putUInt]] to encode values that are positive.
    *
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @note Have to be decoded '''only''' with [[ByteBufferReader.getInt]]
    *       The resulting varint uses ZigZag encoding, which is much more efficient at
    *       encoding negative values than pure VLQ.
    * @param x prefer signed Int
    */
  @inline override def putInt(x: Int): this.type = putULong(encodeZigZagInt(x))

  /**
    * Encode unsigned Int value using VLQ.
    * Only positive values are supported. Use [[putInt]]
    * to encode negative and positive values.
    *
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @param x unsigned Int
    * @throws AssertionError for values not in unsigned Int range
    */
  @inline override def putUInt(x: Long): this.type = {
    assert(x >= 0 && x <= 0xFFFFFFFFL, s"$x is out of unsigned int range")
    putULong(x)
  }

  /**
    * Encode signed Long using VLQ with ZigZag.
    * Both negative and positive values are supported, but due to ZigZag encoding positive
    * values is done less efficiently than by [[putULong]].
    * Use [[putULong]] to encode values that are positive.
    *
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @note Have to be decoded '''only''' with [[ByteBufferReader.getLong]]
    *       The resulting varint uses ZigZag encoding, which is much more efficient at
    *       encoding negative values than pure VLQ.
    * @param x prefer signed Long
    */
  @inline override def putLong(x: Long): this.type = putULong(encodeZigZagLong(x))

  /**
    * Encode signed Long value using VLQ.
    * Both negative and positive values are supported, but only positive values are encoded
    * efficiently, negative values are taking a toll and use six bytes. Use [[putLong]]
    * to encode negative and positive values.
    *
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @note Don't use it for negative values, the resulting varint is always ten
    *       bytes long – it is, effectively, treated like a very large unsigned integer.
    *       If you use [[putLong]], the resulting varint uses ZigZag encoding,
    *       which is much more efficient.
    * @param x prefer unsigned Long (signed value will produce a significant overhead,
    *          see note above)
    */
  @inline override def putULong(x: Long): this.type = {
    val buffer = new Array[Byte](10)  // TODO optimize allocation by removing this buffer, it seems to not necessary
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

  @inline override def putBytes(xs: Array[Byte]): this.type = { b.append(xs); this }

  @inline override def putBits(xs: Array[Boolean]): this.type = {
    if (xs.isEmpty) return this
    val bitSet = new BitSet(xs.length)
    xs.zipWithIndex.foreach { case (bool, i) => bitSet.set(i, bool)}
    // pad the byte array to fix the "no bit was set" behaviour
    // see https://stackoverflow.com/questions/11209600/how-do-i-convert-a-bitset-initialized-with-false-in-a-byte-containing-0-in-java
    val bytes = Arrays.copyOf(bitSet.toByteArray, (xs.length + 7) / 8)
    b.append(bytes)
    this
  }

  @inline override def putOption[T](x: Option[T])(putValue: (this.type, T) => Unit): this.type = { b.appendOption(x)(v => putValue(this, v)); this }

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
