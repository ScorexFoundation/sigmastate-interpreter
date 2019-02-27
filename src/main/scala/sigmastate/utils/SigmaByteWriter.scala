package sigmastate.utils

import sigma.util.ByteArrayBuilder
import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.{TypeSerializer, ValueSerializer, ConstantStore}

trait SigmaByteWriter extends ByteWriter {
  def putType[T <: SType](x: T): SigmaByteWriter
  def putValue[T <: SType](x: Value[T]): SigmaByteWriter
  def putValues[T <: SType](xs: Seq[Value[T]]): SigmaByteWriter
  val constantExtractionStore: Option[ConstantStore]
}

class SigmaByteWriterC(b: ByteArrayBuilder, val constantExtractionStore: Option[ConstantStore]) extends ByteArrayWriter(b) with SigmaByteWriter {

  @inline override def putType[T <: SType](x: T): SigmaByteWriter = { TypeSerializer.serialize(x, this); this }
  @inline override def putValue[T <: SType](x: Value[T]): SigmaByteWriter = { ValueSerializer.serialize(x, this); this }
  @inline override def putValues[T <: SType](xs: Seq[Value[T]]): SigmaByteWriter = {
    putUInt(xs.length)
    xs.foreach(putValue(_))
    this
  }
}

class SigmaByteWriterWithLog(b: ByteArrayBuilder, val constantExtractionStore: Option[ConstantStore]) extends ByteArrayWriterWithLog(b) with SigmaByteWriter {

  @inline override def putType[T <: SType](x: T): SigmaByteWriter = { TypeSerializer.serialize(x, this); this }
  @inline override def putValue[T <: SType](x: Value[T]): SigmaByteWriter = {

    val s = "0x%02X".format(x.opCode).mkString("")

    SerializeLog.logPrintf(true, true, "Value [opCode=" + s + "]")

    ValueSerializer.serialize(x, this)

    SerializeLog.logPrintf(false, true, "Value [opCode=" + s + "]")

    this
  }
  @inline override def putValues[T <: SType](xs: Seq[Value[T]]): SigmaByteWriter = {
    putUInt(xs.length)
    xs.foreach(putValue(_))
    this
  }
}