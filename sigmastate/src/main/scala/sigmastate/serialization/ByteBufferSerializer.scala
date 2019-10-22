package sigmastate.serialization

import sigmastate.utils.{SigmaByteWriter, SigmaByteReader}

/** Interface of serializers which use ByteWriter to serialize and ByteReader to deserialize. */
trait ByteBufferSerializer[T] {
  def serialize(value: T, w: SigmaByteWriter): Unit
  def deserialize(r: SigmaByteReader): T
}
