package sigmastate.serialization

import sigmastate.utils.{ByteWriter, ByteReader}

/** Interface of serializers which use ByteWriter to serialize and ByteReader to deserialize. */
trait ByteBufferSerializer[T] {
  def serialize(tpe: T, buf: ByteWriter): Unit
  def deserialize(buf: ByteReader): T
}
