package sigmastate.utils

import java.nio.ByteBuffer
import sigmastate.Values.SValue
import sigmastate.SType
import sigmastate.utils.Extensions._
import sigmastate.serialization.TypeSerializer

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
  @inline override def getLong(): Long = buf.getLong()
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


