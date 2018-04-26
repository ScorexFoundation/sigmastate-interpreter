package sigmastate.serialization

import sigmastate.SType.TypeCode
import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.ValueSerializer.Position

case class Relation3Serializer[S1 <: SType, S2 <: SType, S3 <: SType, R <: Relation3[S1, S2, S3]]
  (override val opCode: Byte,
   cons: (Value[S1], Value[S2], Value[S3]) => R) extends ValueSerializer[R] {

  import ValueSerializer.{deserialize, serialize}

  val typeCode: TypeCode = SBoolean.typeCode

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    val (firstArg, consumed) = deserialize(bytes, pos)
    val (secondArg, consumed2) = deserialize(bytes, pos + consumed)
    val (thirdArg, consumed3) = deserialize(bytes, pos + consumed + consumed2)
    val result = cons(firstArg.asInstanceOf[Value[S1]], secondArg.asInstanceOf[Value[S2]], thirdArg.asInstanceOf[Value[S3]])
    val tConsumed = consumed + consumed2 + consumed3
    result -> tConsumed
  }

  override def serializeBody(rel: R) = {
    serialize(rel.first) ++ serialize(rel.second) ++ serialize(rel.third)
  }
}