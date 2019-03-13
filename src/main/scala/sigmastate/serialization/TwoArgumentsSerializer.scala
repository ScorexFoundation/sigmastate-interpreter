package sigmastate.serialization

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.{SBigInt, SType, TwoArgumentsOperation}

case class TwoArgumentsSerializer[LIV <: SType, RIV <: SType, OV <: Value[SType]]
(override val opCode: Byte, constructor: (Value[LIV], Value[RIV]) => Value[SType])
  extends ValueSerializer[OV] {

  override def serializeBody(obj: OV, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false,"TwoArguments operation")

    val typedOp = obj.asInstanceOf[TwoArgumentsOperation[LIV, RIV, LIV]]

    SerializeLog.logPrintf(true, true, false,"Left")
    w.putValue(typedOp.left)
    SerializeLog.logPrintf(false, true, false,"Left")

    SerializeLog.logPrintf(true, true, false,"Right")
    w.putValue(typedOp.right)
    SerializeLog.logPrintf(false, true, false,"Right")

    SerializeLog.logPrintf(false, true, false,"TwoArguments operation")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val arg1 = r.getValue().asValue[LIV]
    val arg2 = r.getValue().asValue[RIV]
    constructor(arg1, arg2)
  }
}
