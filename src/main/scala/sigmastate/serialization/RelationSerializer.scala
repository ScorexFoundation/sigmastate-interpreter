package sigmastate.serialization

import sigmastate.SType.TypeCode
import sigmastate._

case class RelationSerializer[S1 <: SType, S2 <: SType, R <: Relation[S1, S2]]
  (override val opCode: Byte,
   constructor: (Value[S1], Value[S2]) => R,
   constraints: Seq[Constraints.Contraint2]) extends

  SigmaSerializer[R]{

  import SigmaSerializer.{serialize, deserialize}

  val typeCode: TypeCode = SBoolean.typeCode

  override def parseBody = {
    case (bytes, pos) =>
      val (firstArg, consumed, tc1) = deserialize(bytes, pos)
      val (secondArg, consumed2, tc2) = deserialize(bytes, pos + consumed)
      assert(constraints.forall(c => c(tc1, tc2)))
      (constructor(firstArg.asInstanceOf[Value[S1]], secondArg.asInstanceOf[Value[S2]]),
        (consumed + consumed2),
        typeCode)
  }

  override def serializeBody = {rel =>
    serialize(rel.left) ++ serialize(rel.right)
  }
}