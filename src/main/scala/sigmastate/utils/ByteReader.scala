package sigmastate.utils

import java.nio.ByteBuffer

import sigmastate.Values.SValue
import sigmastate.SType
import sigmastate.utils.Extensions._
import sigmastate.serialization.TypeSerializer
import sigmastate.utils.ByteBufferReader.decodeZigZagLong

trait ByteReader {
  def getByte(): Byte
  def getUByte(): Int
  def getShort(): Short
  def getInt(): Int
  def getLong(): Long
  def getBytes(size: Int): Array[Byte]
  def getOption[T](getValue: => T): Option[T]
  def getType(): SType
  def getValue(): SValue
  def mark(): ByteReader
  def consumed: Int
  def position: Int
  def position_=(p: Int)
  def remaining: Int
}

class ByteBufferReader(buf: ByteBuffer) extends ByteReader {
  @inline override def getByte(): Byte = buf.get
  @inline override def getUByte(): Int = buf.get & 0xFF
  @inline override def getShort(): Short = buf.getShort()
  @inline override def getInt(): Int = buf.getInt()

  /**
    * Decode signed Int previously encoded with [[ByteArrayWriter.putSInt]] using VLQ.
    *
    * @note Uses ZigZag encoding. Should be used to decode '''only''' a value that was previously
    *       encoded with [[ByteArrayWriter.putSInt]].
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @return signed Int
    */
  @inline def getSInt(): Int = ByteBufferReader.decodeZigZagInt(getULong().toInt)

  /**
    * Decode Int previously encoded with [[ByteArrayWriter.putUInt]] using VLQ.
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @return Int
    */
  @inline def getUInt(): Int = getULong().toInt

  @inline override def getLong(): Long = buf.getLong()

  /**
    * Decode signed Long previously encoded with [[ByteArrayWriter.putSLong]] using VLQ.
    *
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @note Uses ZigZag encoding. Should be used to decode '''only''' a value that was previously
    *       encoded with [[ByteArrayWriter.putSLong]].
    * @return signed Long
    */
  @inline def getSLong(): Long = decodeZigZagLong(getULong())

  /**
    * Decode Long previously encoded with [[ByteArrayWriter.putULong]] using VLQ.
    *
    * @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
    * @return Long
    */
  @inline def getULong(): Long = {
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
  }

  @inline override def getBytes(size: Int): Array[Byte] = buf.getBytes(size)
  @inline override def getOption[T](getValue: => T): Option[T] = buf.getOption(getValue)
  @inline override def getType(): SType = TypeSerializer.deserialize(this)
  @inline override def getValue(): SValue = buf.getValue

  private var _mark: Int = _
  @inline override def mark(): ByteReader = {
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
