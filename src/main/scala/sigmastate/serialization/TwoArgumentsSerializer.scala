package sigmastate.serialization

import sigmastate.SType.TypeCode
import sigmastate.{SType, TwoArgumentsOperation}
import sigmastate.Values.Value
import sigmastate.serialization.ValueSerializer.{deserialize, serialize}
import sigmastate.serialization.Serializer.{Position, Consumed}


case class TwoArgumentsSerializer[LIV <: SType, RIV <: SType, OV <: Value[SType]]
(override val opCode: Byte, constructor: (Value[LIV], Value[RIV]) => Value[SType])
  extends ValueSerializer[OV] {

  override def parseBody(bytes: Array[TypeCode], pos: Position): (Value[SType], Consumed) = {
    val (firstArg, consumed) = deserialize(bytes, pos)
    val (secondArg, consumed2) = deserialize(bytes, pos + consumed)

    (constructor(firstArg.asInstanceOf[Value[LIV]], secondArg.asInstanceOf[Value[RIV]]), consumed + consumed2)
  }

  override def serializeBody(operation: OV): Array[TypeCode] = {
    val typedOp = operation.asInstanceOf[TwoArgumentsOperation[LIV, RIV, LIV]]
    serialize(typedOp.left) ++ serialize(typedOp.right)
  }
}
