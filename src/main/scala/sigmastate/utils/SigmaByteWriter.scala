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
  @inline def putLong(x: Long, info: DataInfo[Long]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putLong(x); this
  }

  @inline def putULong(x: Long): this.type = { w.putULong(x); this }
  @inline def putULong(x: Long, info: DataInfo[U[Long]]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putULong(x); this
  }

  @inline def putBytes(xs: Array[Byte]): this.type = { w.putBytes(xs); this }
  @inline def putBytes(xs: Array[Byte], info: DataInfo[Array[Byte]]): this.type = {
    ValueSerializer.addArgInfo(info)
    w.putBytes(xs); this
  }

  @inline def putBits(xs: Array[Boolean]): this.type = { w.putBits(xs); this }
  @inline def putBits(xs: Array[Boolean], info: DataInfo[Array[Boolean]]): this.type = {
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
  trait FormatDescriptor[T]

  /** Marker type to automatically resolve correct implicit format descriptor
    * in Writer methods.
    * This is phantom type, since no instances of it are ever created. */
  trait Vlq[T]

  /** Marker type for Unsigned types to automatically resolve correct implicit format descriptor
    * in Writer methods.
    * This is phantom type, since no instances of it are ever created. */
  trait U[T]

  implicit case object ValueFmt extends FormatDescriptor[SValue]
  implicit case object TypeFmt extends FormatDescriptor[SType]
  implicit case object BitsFmt extends FormatDescriptor[Array[Boolean]]

  implicit object BooleanFmt extends FormatDescriptor[Boolean]
  implicit object ByteFmt extends FormatDescriptor[Byte]
  implicit object ShortFmt extends FormatDescriptor[Short]
  implicit object IntFmt extends FormatDescriptor[Int]
  implicit object LongFmt extends FormatDescriptor[Long]

  implicit object UByteFmt extends FormatDescriptor[U[Byte]]
  implicit object UShortFmt extends FormatDescriptor[U[Short]]
  implicit object UIntFmt extends FormatDescriptor[U[Int]]
  implicit object ULongFmt extends FormatDescriptor[U[Long]]

  case class DataInfo[T](info: ArgInfo, format: FormatDescriptor[T])

  implicit def argInfoToDataInfo[T](arg: ArgInfo)(implicit fmt: FormatDescriptor[T]): DataInfo[T] = DataInfo(arg, fmt)

  // TODO remove this conversion and make it explicit
  /**Helper conversion */
  implicit def nameToDataInfo[T](name: String)(implicit fmt: FormatDescriptor[T]): DataInfo[T] = ArgInfo(name, "")
}
