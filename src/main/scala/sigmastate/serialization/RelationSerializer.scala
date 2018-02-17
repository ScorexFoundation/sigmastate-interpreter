package sigmastate.serialization

import sigmastate.SType.TypeCode
import sigmastate._

case class RelationSerializer[R <: Relation[_ <: SType, _<: SType]](override val opCode: Byte) extends
  SigmaSerializer[R]{

  import SigmaSerializer.{serialize, deserialize}

  override val typeCode: TypeCode = SBoolean.typeCode

  override def parseBody = {
    case (bytes, pos) =>
      val (firstArg, consumed, tc1) = deserialize(bytes, pos)
      val (secondArg, consumed2, tc2) = deserialize(bytes, pos + consumed)
      assert(tc1 == SInt.typeCode)
      assert(tc2 == SInt.typeCode)
      GE(firstArg.asInstanceOf[Value[SInt.type]], secondArg.asInstanceOf[Value[SInt.type]]) -> (consumed + consumed2)
  }

  override def serializeBody = {rel => serialize(rel.left) ++ serialize(rel.right)}
}
