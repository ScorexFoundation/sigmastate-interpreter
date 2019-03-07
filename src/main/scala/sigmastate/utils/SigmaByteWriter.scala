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

  override def putType[T <: SType](x: T): SigmaByteWriter = {

    val s1 = x.toString()
    val s2 = "0x%02X".format(x.typeCode).mkString("")

    SerializeLog.logPrintf(true, true, false, "Type [toString=" + s1 + "; typeCode="+ s2+ "]")

    TypeSerializer.serialize(x, this);

    SerializeLog.logPrintf(false, true, false, "Type [toString=" + s1 + "; typeCode="+ s2+ "]")

    this
  }

  override def putValue[T <: SType](x: Value[T]): SigmaByteWriter = {

    val s1 = x.tpe.toString()
    val s2 = "0x%02X".format(x.opCode).mkString("")

    SerializeLog.logPrintf(true, true, false, "Value [toString=" + s1 + "; opCode="+ s2+ "]")

    ValueSerializer.serialize(x, this)

    SerializeLog.logPrintf(false, true, false, "Value [toString=" + s1 + "; opCode="+ s2+ "]")

    this
  }

  override def putValues[T <: SType](xs: Seq[Value[T]]): SigmaByteWriter = {

    SerializeLog.logPrintf(true, true, false, "Values")

    SerializeLog.logPrintf(true, true, false, "Values length")

    putUInt(xs.length)

    SerializeLog.logPrintf(false, true, false, "Values length")

    xs.foreach(putValue(_))

    SerializeLog.logPrintf(false, true, false, "Values")

    this
  }
}