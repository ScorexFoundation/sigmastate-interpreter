package sigmastate.utils

import scorex.util.serialization.{VLQByteStringWriter, VLQByteBufferWriter, Writer}
import scorex.util.serialization.Writer.Aux
import sigmastate.{ArgInfo, SType}
import sigmastate.Values.{Value, SValue}
import sigmastate.serialization.{TypeSerializer, ValueSerializer, ConstantStore}
import sigmastate.utils.SigmaByteWriter.ValueFmt

class SigmaByteWriter(val w: Writer,
                      val constantExtractionStore: Option[ConstantStore]) extends Writer {
  import SigmaByteWriter._
  type CH = w.CH

  @inline override def length(): Int = w.length()

  @inline override def newWriter(): Aux[CH] = w.newWriter()

  @inline override def putChunk(chunk: CH): this.type = { w.putChunk(chunk); this }

  @inline override def result(): CH = w.result()

  @inline def put(x: Byte): this.type = { w.put(x); this }
  @inline def put(x: Byte, info: DataInfo[Byte]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.put(x); this
  }

  override def putUByte(x: Int): this.type = {
    super.putUByte(x)
  }
  def putUByte(x: Int, info: DataInfo[U[Byte]]): this.type = {
    ValueSerializer.addArgInfo(info)
    super.putUByte(x)
  }

  @inline def putBoolean(x: Boolean): this.type = { w.putBoolean(x); this }
  @inline def putBoolean(x: Boolean, info: DataInfo[Boolean]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putBoolean(x); this
  }

  @inline def putShort(x: Short): this.type = { w.putShort(x); this }
  @inline def putShort(x: Short, info: DataInfo[Short]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putShort(x); this
  }

  @inline def putUShort(x: Int): this.type = { w.putUShort(x); this }
  @inline def putUShort(x: Int, info: DataInfo[U[Short]]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putUShort(x); this
  }

  @inline def putInt(x: Int): this.type = { w.putInt(x); this }
  @inline def putInt(x: Int, info: DataInfo[Int]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putInt(x); this
  }

  @inline def putUInt(x: Long): this.type = { w.putUInt(x); this }
  @inline def putUInt(x: Long, info: DataInfo[U[Int]]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putUInt(x); this
  }

  @inline def putLong(x: Long): this.type = { w.putLong(x); this }
  @inline def putLong(x: Long, info: DataInfo[Vlq[ZigZag[Long]]]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putLong(x); this
  }

  @inline def putULong(x: Long): this.type = { w.putULong(x); this }
  @inline def putULong(x: Long, info: DataInfo[Vlq[U[Long]]]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putULong(x); this
  }

  @inline def putBytes(xs: Array[Byte]): this.type = { w.putBytes(xs); this }
  @inline def putBytes(xs: Array[Byte], info: DataInfo[Array[Byte]]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putBytes(xs); this
  }

  @inline def putBits(xs: Array[Boolean]): this.type = { w.putBits(xs); this }
  @inline def putBits(xs: Array[Boolean], info: DataInfo[Bits]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putBits(xs);
    this
  }

  @inline def putOption[T](x: Option[T])(putValueC: (this.type, T) => Unit): this.type = {
    w.putOption(x) { (_, v) =>
      putValueC(this, v)
    }
    this
  }

  @inline def putShortString(s: String): this.type = { w.putShortString(s); this }

  // todo move to Writer
  @inline def toBytes: Array[Byte] = w match {
    case wr: VLQByteStringWriter => wr.result().asByteBuffer.array()
    case wr: VLQByteBufferWriter => wr.toBytes
  }

  @inline def putType[T <: SType](x: T): this.type = { TypeSerializer.serialize(x, this); this }
  @inline def putType[T <: SType](x: T, info: DataInfo[SType]): this.type = {
    ValueSerializer.addArgInfo(info)
    TypeSerializer.serialize(x, this); this
  }

  @inline def putValue[T <: SType](x: Value[T]): this.type = { ValueSerializer.serialize(x, this); this }
  @inline def putValue[T <: SType](x: Value[T], info: DataInfo[SValue]): this.type = {
    ValueSerializer.addArgInfo(info)
    ValueSerializer.serialize(x, this); this
  }
  @inline def putValues[T <: SType](xs: Seq[Value[T]]): this.type = {
    putUInt(xs.length)
    xs.foreach(putValue(_))
    this
  }

}

object SigmaByteWriter {
  import scala.language.implicitConversions

  /** Format descriptor type family. */
  trait FormatDescriptor[T] {
    /** Size formula associated with this format */
    def size: String
  }

  /** Marker type to automatically resolve correct implicit format descriptor
    * in Writer methods.
    * This is type-level type, since no instances of it are ever created. */
  trait Vlq[T]

  /** Marker type to automatically resolve correct implicit format descriptor
    * in Writer methods.
    * This is type-level type, since no instances of it are ever created. */
  trait ZigZag[T]

  /** Marker type for Unsigned types to automatically resolve correct implicit format descriptor
    * in Writer methods.
    * This is type-level type, since no instances of it are ever created. */
  trait U[T]

  /** Marker type for bits representation of Coll[Boolean].
    * Should be used only as argument for FormatDescriptor.
    * This is type-level type, since no instances of it are ever created.
    */
  trait Bits

  implicit case object ValueFmt extends FormatDescriptor[SValue] {
    override def size: String = "[1, *]"
    override def toString: String = "Value"
  }
  implicit case object TypeFmt extends FormatDescriptor[SType] {
    override def size: String = "[1, *]"
    override def toString: String = "Type"
  }

  case object BitsFmt extends FormatDescriptor[Bits] {
    override def size: String = "[1, *]"
    override def toString: String = "Bits"
  }

  case class MaxBitsFmt(maxBits: Int) extends FormatDescriptor[Bits] {
    override def size: String = {
      val maxBytes = (maxBits - 1) / 8 + 1
      if (maxBytes == 1) "1"
      else s"[1, $maxBytes]"
    }
    override def toString: String = "Bits"
  }

  implicit object BooleanFmt extends FormatDescriptor[Boolean] {
    override def size: String = "1"
    override def toString: String = "Boolean"
  }
  implicit object ByteFmt extends FormatDescriptor[Byte] {
    override def size: String = "1"
    override def toString: String = "Byte"
  }
  implicit object ShortFmt extends FormatDescriptor[Short] {
    override def size: String = "2"
    override def toString: String = "Short"
  }
  implicit object IntFmt extends FormatDescriptor[Int] {
    override def size: String = "4"
    override def toString: String = "Int"
  }
  implicit object LongFmt extends FormatDescriptor[Long] {
    override def size: String = "8"
    override def toString: String = "Long"
  }

  implicit object UByteFmt extends FormatDescriptor[U[Byte]] {
    override def size: String = "1"
    override def toString: String = "UByte"
  }
  implicit object UShortFmt extends FormatDescriptor[U[Short]] {
    override def size: String = "2"
    override def toString: String = "UShort"
  }
  implicit object UIntFmt extends FormatDescriptor[U[Int]] {
    override def size: String = "4"
    override def toString: String = "UInt"
  }
  implicit object ULongFmt extends FormatDescriptor[U[Long]] {
    override def size: String = "8"
    override def toString: String = "ULong"
  }

  case class ZigZagFmt[T](fmt: FormatDescriptor[T]) extends FormatDescriptor[ZigZag[T]] {
    override def size: String = s"[1, *]"
    override def toString: String = s"ZigZag($fmt)"
  }
  case class UVlqFmt[T](fmt: FormatDescriptor[U[T]]) extends FormatDescriptor[Vlq[U[T]]] {
    override def size: String = s"[1, *]"
    override def toString: String = s"VLQ($fmt)"
  }
  case class ZigZagVlqFmt[T](fmt: FormatDescriptor[ZigZag[T]]) extends FormatDescriptor[Vlq[ZigZag[T]]] {
    override def size: String = s"[1, *]"
    override def toString: String = s"VLQ($fmt)"
  }

  implicit def toZigZagFmt[T](implicit fmt: FormatDescriptor[T]): FormatDescriptor[ZigZag[T]] = ZigZagFmt(fmt)
  implicit def toUVlqFmt[T](implicit fmt: FormatDescriptor[U[T]]): FormatDescriptor[Vlq[U[T]]] = UVlqFmt(fmt)
  implicit def toZigZagVlqFmt[T](implicit fmt: FormatDescriptor[ZigZag[T]]): FormatDescriptor[Vlq[ZigZag[T]]] = ZigZagVlqFmt(fmt)

  case class DataInfo[T](info: ArgInfo, format: FormatDescriptor[T])

  implicit def argInfoToDataInfo[T](arg: ArgInfo)(implicit fmt: FormatDescriptor[T]): DataInfo[T] = DataInfo(arg, fmt)

  def bitsInfo(name: String, desc: String = ""): DataInfo[Bits] = DataInfo(ArgInfo(name, desc), BitsFmt)
  def maxBitsInfo(name: String, maxBits: Int, desc: String = ""): DataInfo[Bits] = DataInfo(ArgInfo(name, desc), MaxBitsFmt(maxBits))

  // TODO remove this conversion and make it explicit
  /**Helper conversion */
  implicit def nameToDataInfo[T](name: String)(implicit fmt: FormatDescriptor[T]): DataInfo[T] = ArgInfo(name, "")
}
