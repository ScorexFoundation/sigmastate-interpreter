package sigmastate.serialization

import sigmastate.Values.{BigIntValue, Value, ValueCompanion}
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{SType, ModQArithOp, ModQArithOpCompanion}

case class ModQArithOpSerializer(override val opDesc: ModQArithOpCompanion, cons: (BigIntValue, BigIntValue) => BigIntValue)
  extends ValueSerializer[ModQArithOp] {

  override def serialize(obj: ModQArithOp, w: SigmaByteWriter): Unit = {
    w.putValue(obj.left, opDesc.argInfos(0))
      .putValue(obj.right, opDesc.argInfos(1))
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val arg1 = r.getValue().asBigInt
    val arg2 = r.getValue().asBigInt
    cons(arg1, arg2)
  }
}
