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

  @inline def putBoolean(x: Boolean): this.type = { w.putBoolean(x); this }

  @inline def putShort(x: Short): this.type = { w.putShort(x); this }

  @inline def putUShort(x: Int): this.type = { w.putUShort(x); this }

  @inline def putInt(x: Int): this.type = { w.putInt(x); this }

  @inline def putUInt(x: Long): this.type = { w.putUInt(x); this }

  @inline def putLong(x: Long): this.type = { w.putLong(x); this }

  @inline def putULong(x: Long): this.type = { w.putULong(x); this }

  @inline def putBytes(xs: Array[Byte]): this.type = { w.putBytes(xs); this }

  @inline def putBits(xs: Array[Boolean]): this.type = { w.putBits(xs); this }

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

  implicit case object ValueFmt extends FormatDescriptor[SValue]
  object ByteFmt extends FormatDescriptor[Byte]
  object IntFmt extends FormatDescriptor[Int]
  object VlqIntFmt extends FormatDescriptor[Vlq[Int]]

  case class DataInfo[T](info: ArgInfo, format: FormatDescriptor[T])

  implicit def argInfoToDataInfo[T](arg: ArgInfo)(implicit fmt: FormatDescriptor[T]): DataInfo[T] = DataInfo(arg, fmt)
}
