package sigmastate.utils

import java.nio.ByteBuffer

import sigmastate.SType
import sigmastate.Values.SValue
import sigmastate.serialization.{TypeSerializer, ValueSerializer}

trait ByteReaderSigmaValues extends ByteReader {

  def getType(): SType
  def getValue(): SValue
}

class ByteBufferReaderSigmaValues(buf: ByteBuffer) extends ByteBufferReader(buf)
  with ByteReaderSigmaValues {

  @inline override def getType(): SType = TypeSerializer.deserialize(this)
  @inline override def getValue(): SValue = ValueSerializer.deserialize(this)
}
