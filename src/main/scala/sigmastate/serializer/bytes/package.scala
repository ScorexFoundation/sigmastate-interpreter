package sigmastate.serializer

import scorex.core.serialization.Serializer

package object bytes {
  def seqBytesSerializer[BS <: BytesSerializable](implicit bs: Serializer[BS]) = new SeqBytesSerializer[BS]

  implicit val heightSerializer: HeightSerializer = new HeightSerializer
  implicit val intConstantSerializer: IntConstantSerializer = new IntConstantSerializer
  implicit val sIntSerializer: SIntSerializer = new SIntSerializer
}
