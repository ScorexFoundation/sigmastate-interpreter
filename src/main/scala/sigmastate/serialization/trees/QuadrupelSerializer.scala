package sigmastate.serialization.trees

import sigmastate.Values._
import sigmastate.{Quadruple, _}
import sigmastate.serialization.ValueSerializer
import sigmastate.serialization.Serializer.Position

case class QuadrupelSerializer[S1 <: SType, S2 <: SType, S3 <: SType, S4 <: SType]
(override val opCode: Byte,
 cons: (Value[S1], Value[S2], Value[S3]) => Quadruple[S1, S2, S3, S4]) extends ValueSerializer[Quadruple[S1, S2, S3, S4]] {

  import ValueSerializer.{deserialize, serialize}

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    val (firstArg, consumed) = deserialize(bytes, pos)
    val (secondArg, consumed2) = deserialize(bytes, pos + consumed)
    val (thirdArg, consumed3) = deserialize(bytes, pos + consumed + consumed2)
    val result = cons(firstArg.asInstanceOf[Value[S1]], secondArg.asInstanceOf[Value[S2]], thirdArg.asInstanceOf[Value[S3]])
    val tConsumed = consumed + consumed2 + consumed3
    result -> tConsumed
  }

  override def serializeBody(rel: Quadruple[S1, S2, S3, S4]) = {
    serialize(rel.first) ++ serialize(rel.second) ++ serialize(rel.third)
  }
}