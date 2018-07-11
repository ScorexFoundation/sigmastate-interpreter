package sigmastate.serialization

import sigmastate.utils.{ByteWriterSigmaValues, ByteReader}

/** Interface of serializers which use ByteWriter to serialize and ByteReader to deserialize. */
trait ByteBufferSerializer[T] {
  def serialize(value: T, w: ByteWriterSigmaValues): Unit
  def deserialize(r: ByteReader): T
}
