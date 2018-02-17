package sigmastate.serialization

import sigmastate._

case class RelationSerializer[R <: Relation[_ <: SType, _<: SType]](override val opCode: Byte) extends
  SigmaSerializer[R]{

  import SigmaSerializer.deserialize

  override def parseBody = {
    case (bytes, pos) =>
      val (firstArg, consumed) = deserialize(bytes, pos)
      val (secondArg, consumed2) = deserialize(bytes, pos + consumed)
      GE(firstArg.asInstanceOf[Value[SInt.type]], secondArg.asInstanceOf[Value[SInt.type]]) -> (consumed + consumed2)
  }

  override def serializeBody = {cc => ???}
}
