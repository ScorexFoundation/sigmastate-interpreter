package sigmastate.serialization

import sigmastate.Values.{BigIntValue, Value, ValueCompanion}
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{SType, ModQArithOp}

case class ModQArithOpSerializer(override val opDesc: ValueCompanion, cons: (BigIntValue, BigIntValue) => BigIntValue)
  extends ValueSerializer[ModQArithOp] {

  override def serialize(obj: ModQArithOp, w: SigmaByteWriter): Unit = {
    w.putValue(obj.left)
      .putValue(obj.right)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val arg1 = r.getValue().asBigInt
    val arg2 = r.getValue().asBigInt
    cons(arg1, arg2)
  }
}
