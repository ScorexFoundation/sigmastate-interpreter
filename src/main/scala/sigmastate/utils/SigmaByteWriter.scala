package sigmastate.utils

import scorex.util.serialization.{ByteStringWriter, VLQByteBufferWriter, Writer}
import scorex.util.serialization.Writer.Aux
import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.{ConstantStore, TypeSerializer, ValueSerializer}

class SigmaByteWriter(val w: Writer,
                      val constantExtractionStore: Option[ConstantStore]) extends Writer {

  type CH = w.CH

  override def length(): Int = w.length()

  override def newWriter(): Aux[CH] = w.newWriter()

  override def putChunk(chunk: CH): this.type = { w.putChunk(chunk); this }

  override def result(): CH = w.result()

  def put(x: Byte): this.type = { w.put(x); this }

  def putBoolean(x: Boolean): this.type = { w.putBoolean(x); this }

  def putShort(x: Short): this.type = { w.putShort(x); this }

  def putUShort(x: Int): this.type = { w.putUShort(x); this }

  def putInt(x: Int): this.type = { w.putInt(x); this }

  def putUInt(x: Long): this.type = { w.putUInt(x); this }

  def putLong(x: Long): this.type = { w.putLong(x); this }

  def putULong(x: Long): this.type = { w.putULong(x); this }

  def putBytes(xs: Array[Byte]): this.type = { w.putBytes(xs); this }

  def putBits(xs: Array[Boolean]): this.type = { w.putBits(xs); this }

  def putOption[T](x: Option[T])(putValueC: (this.type, T) => Unit): this.type = {
    w.putOption(x) { (_, v) =>
      putValueC(this, v)
    }
    this
  }

  def putShortString(s: String): this.type = { w.putShortString(s); this }

  // todo move to Writer
  def toBytes: Array[Byte] = w match {
    case wr: ByteStringWriter => wr.result().asByteBuffer.array()
    case wr: VLQByteBufferWriter => wr.toBytes
  }

  @inline def putType[T <: SType](x: T): this.type = { TypeSerializer.serialize(x, this); this }
  @inline def putValue[T <: SType](x: Value[T]): this.type = { ValueSerializer.serialize(x, this); this }
  @inline def putValues[T <: SType](xs: Seq[Value[T]]): this.type = {
    putUInt(xs.length)
    xs.foreach(putValue(_))
    this
  }
}
