package sigmastate.utils

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.{TypeSerializer, ValueSerializer}

trait ByteWriterSigmaValues extends ByteWriter {

  def putType[T <: SType](x: T): ByteWriterSigmaValues
  def putValue[T <: SType](x: Value[T]): ByteWriterSigmaValues
}

class ByteArrayWriterSigmaValues(b: ByteArrayBuilder) extends ByteArrayWriter(b)
  with ByteWriterSigmaValues {

  @inline override def putType[T <: SType](x: T): ByteWriterSigmaValues = {
    TypeSerializer.serialize(x, this)
    this
  }

  @inline override def putValue[T <: SType](x: Value[T]): ByteWriterSigmaValues = {
    ValueSerializer.serialize(x, this)
    this
  }
}
