package sigmastate.serialization

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.{SType, TwoArgumentsOperation}


case class TwoArgumentsSerializer[LIV <: SType, RIV <: SType, OV <: Value[SType]]
(override val opCode: Byte, constructor: (Value[LIV], Value[RIV]) => Value[SType])
  extends ValueSerializer[OV] {

  override def parseBody(r: ByteReader): Value[SType] = {
    val arg1 = r.getValue().asValue[ArgType1]
    val arg2 = r.getValue().asValue[ArgType2]
    constructor(arg1, arg2)
  }

  override def serializeBody(obj: Operation, w: ByteWriter): Unit =
    w.putValue(obj.left)
      .putValue(obj.right)
  override def serializeBody(operation: OV): Array[TypeCode] = {
    val typedOp = operation.asInstanceOf[TwoArgumentsOperation[LIV, RIV, LIV]]
    serialize(typedOp.left) ++ serialize(typedOp.right)
  }
}
