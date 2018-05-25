package sigmastate.utils

import java.nio.ByteBuffer

import sigmastate.Values.{Value, SValue}
import sigmastate.SType
import Extensions._

trait ByteReader {
  def get(): Byte
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
}

class ByteBufferReader(buf: ByteBuffer) extends ByteReader {
  override def get(): Byte = buf.get
  override def getShort(): Short = buf.getShort()
  override def getInt(): Int = buf.getInt()
  override def getLong(): Long = buf.getLong()
  override def getBytes(size: Int): Array[Byte] = buf.getBytes(size)
  override def getOption[T](getValue: => T): Option[T] = buf.getOption(getValue)
  override def getType(): SType = buf.getType
  override def getValue(): SValue = buf.getValue

  private var _mark: Int = _
  override def mark(): ByteReader = {
    _mark = buf.position()
    this
  }
  override def consumed: Int = buf.position() - _mark

  override def position: Int = buf.position()

  override def position_=(p: Int): Unit = buf.position(p)
}


