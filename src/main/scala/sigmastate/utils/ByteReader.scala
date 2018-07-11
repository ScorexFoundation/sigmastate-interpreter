package sigmastate.utils

import java.nio.ByteBuffer
import java.util._

import sigmastate.utils.ByteBufferReader.decodeZigZagLong
import sigmastate.utils.Extensions._

trait ByteReader {

  /**
    * Get a byte at current position without advancing the position.
    * @return byte at current position
    */
  def peekByte(): Byte
  def getByte(): Byte
  def getUByte(): Int
  def getShort(): Short

  /**
    * Decode positive Short.
    * Use '''only''' for values previously encoded with [[ByteArrayWriter.putUShort]]
    * @return signed Int
    */
  def getUShort(): Int

  /**
    * Decode signed Int.
    * Use '''only''' for values previously encoded with [[ByteArrayWriter.putInt]]
    * @return signed Int
    */
  def getInt(): Int

  /**
    * Decode positive Int.
    * Use '''only''' for values previously encoded with [[ByteArrayWriter.putUInt]]
    * @return signed Long
    */
  def getUInt(): Long

  /**
    * Decode signed Long.
    * Use '''only''' for values previously encoded with [[ByteArrayWriter.putLong]]
    * @return signed Long
    */
  def getLong(): Long

  /**
    * Decode positive Long.
    * Use '''only''' for values previously encoded with [[ByteArrayWriter.putULong]]
    * @return signed Long
    */
  def getULong(): Long

  def getBytes(size: Int): Array[Byte]

  /**
    * Decode array of boolean values previously encode with [[ByteArrayWriter.putBits]]
    * @param size expected size of decoded array
    * @return decoded array of boolean values
    */
  def getBits(size: Int): Array[Boolean]
  def getOption[T](getValue: => T): Option[T]
  def mark(): this.type
  def consumed: Int
  def position: Int
  def position_=(p: Int)
  def remaining: Int
}

class ByteBufferReader(buf: ByteBuffer) extends ByteReader {

  @inline override def peekByte(): Byte = buf.array()(buf.position())
  @inline override def getByte(): Byte = buf.get
  @inline override def getUByte(): Int = buf.get & 0xFF
  @inline override def getShort(): Short = buf.getShort()

  /**
    * Decode Short previously encoded with [[ByteArrayWriter.putUShort]] using VLQ.
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @return Int
    */
  @inline override def getUShort(): Int = getUInt().toInt

  /**
    * Decode signed Int previously encoded with [[ByteArrayWriter.putInt]] using VLQ with ZigZag.
    *
    * @note Uses ZigZag encoding. Should be used to decode '''only''' a value that was previously
    *       encoded with [[ByteArrayWriter.putInt]].
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @return signed Int
    */
  @inline override def getInt(): Int =
  // should only be changed simultaneously with `putInt`
    ByteBufferReader.decodeZigZagInt(getULong().toInt)

  /**
    * Decode Int previously encoded with [[ByteArrayWriter.putUInt]] using VLQ.
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @return Long
    */
  @inline override def getUInt(): Long = getULong()

  /**
    * Decode signed Long previously encoded with [[ByteArrayWriter.putLong]] using VLQ with ZigZag.
    *
    * @note Uses ZigZag encoding. Should be used to decode '''only''' a value that was previously
    *       encoded with [[ByteArrayWriter.putLong]].
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @return signed Long
    */
  @inline override def getLong(): Long = decodeZigZagLong(getULong())

  /**
    * Decode Long previously encoded with [[ByteArrayWriter.putULong]] using VLQ.
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @return Long
    */
  @inline override def getULong(): Long = {
    // should be fast if java -> scala conversion did not botched it
    // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L2653
    // for faster version see: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L1085
    var result: Long = 0
    var shift = 0
    while (shift < 64) {
      val b = buf.get()
      result = result | ((b & 0x7F).toLong << shift)
      if ((b & 0x80) == 0) return result
      shift += 7
    }
    sys.error(s"Cannot deserialize Long value. Unexpected buffer $buf with bytes remaining ${buf.getBytes(buf.remaining)}")
    // see https://rosettacode.org/wiki/Variable-length_quantity for implementations in other languages
  }

  @inline override def getBytes(size: Int): Array[Byte] = buf.getBytes(size)

  @inline override def getBits(size: Int): Array[Boolean] = {
    if (size == 0) return Array[Boolean]()
    val bitSet = BitSet.valueOf(buf.getBytes((size + 7) / 8))
    val boolArray = new Array[Boolean](size)
    var i = 0
    while (i < size) {
      boolArray(i) = bitSet.get(i)
      i += 1
    }
    boolArray
  }

  @inline override def getOption[T](getValue: => T): Option[T] = buf.getOption(getValue)

  private var _mark: Int = _
  @inline override def mark(): this.type  = {
    _mark = buf.position()
    this
  }
  @inline override def consumed: Int = buf.position() - _mark

  @inline override def position: Int = buf.position()

  @inline override def position_=(p: Int): Unit = buf.position(p)

  @inline override def remaining: Int = buf.remaining()
}

object ByteBufferReader {

  /**
    * Decode a signed value previously ZigZag-encoded with [[ByteArrayWriter.encodeZigZagInt]]
    *
    * @see [[https://developers.google.com/protocol-buffers/docs/encoding#types]]
    * @param n unsigned Int previously encoded with [[ByteArrayWriter.encodeZigZagInt]]
    * @return signed Int
    */
  def decodeZigZagInt(n: Int): Int =
  // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L553
    (n >>> 1) ^ -(n & 1)

  /**
    * Decode a signed value previously ZigZag-encoded with [[ByteArrayWriter.encodeZigZagLong]]
    *
    * @see [[https://developers.google.com/protocol-buffers/docs/encoding#types]]
    * @param n unsigned Long previously encoded with [[ByteArrayWriter.encodeZigZagLong]]
    * @return signed Long
    */
  def decodeZigZagLong(n: Long): Long =
  // source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L566
    (n >>> 1) ^ -(n & 1)
}
