package sigmastate.serialization

import sigmastate.Values.{BigIntValue, Value}
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{ModQArithOp, SType}

case class ModQArithOpSerializer(override val opCode: Byte, cons: (BigIntValue, BigIntValue) => BigIntValue)
  extends ValueSerializer[ModQArithOp] {

  override def serializeBody(obj: ModQArithOp, w: SigmaByteWriter): Unit = {
    w.putValue(obj.left)
      .putValue(obj.right)
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val arg1 = r.getValue().asBigInt
    val arg2 = r.getValue().asBigInt
    cons(arg1, arg2)
  }
}
