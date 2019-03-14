package sigmastate.serialization

import sigmastate.Values.{BigIntValue, Value}
import sigmastate.lang.Terms._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.{ModQArithOp, SType}

case class ModQArithOpSerializer(override val opCode: Byte, cons: (BigIntValue, BigIntValue) => BigIntValue)
  extends ValueSerializer[ModQArithOp] {

  override def serializeBody(obj: ModQArithOp, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "ModQArithOp")

    SerializeLog.logPrintf(true, true, false, "left")
    w.putValue(obj.left)
    SerializeLog.logPrintf(false, true, false, "left")

    SerializeLog.logPrintf(true, true, false, "right")
    w.putValue(obj.right)
    SerializeLog.logPrintf(false, true, false, "right")

    SerializeLog.logPrintf(false, true, false, "ModQArithOp")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val arg1 = r.getValue().asBigInt
    val arg2 = r.getValue().asBigInt
    cons(arg1, arg2)
  }
}
