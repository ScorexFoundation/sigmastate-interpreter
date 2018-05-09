package sigmastate.serialization

import sigmastate.SType.TypeCode
import sigmastate.Values.Value
import sigmastate.serialization.ValueSerializer.{deserialize, serialize}
import sigmastate.{SType, TwoArgumentsOperation}


case class TwoArgumentsSerializer[ArgType1 <: SType, ArgType2 <: SType, Operation <: TwoArgumentsOperation[ArgType1, ArgType2, ArgType1]](
                                                                                                                                           override val opCode: Byte,
                                                                                                                                           constructor: (Value[ArgType1], Value[ArgType2]) => Operation
                                                                                                                                         ) extends ValueSerializer[Operation] {

  override def parseBody(bytes: Array[TypeCode], pos: companion.Position): (Value[SType], companion.Consumed) = {
    val (firstArg, consumed) = deserialize(bytes, pos)
    val (secondArg, consumed2) = deserialize(bytes, pos + consumed)

    (constructor(firstArg.asInstanceOf[Value[ArgType1]], secondArg.asInstanceOf[Value[ArgType2]]), consumed + consumed2)
  }

  override def serializeBody(operation: Operation): Array[TypeCode] =
    serialize(operation.left) ++ serialize(operation.right)
}
