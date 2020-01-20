package sigmastate.serialization

import sigmastate.Values.{BigIntValue, Value, SValue}
import sigmastate.lang.Terms._
import sigmastate.utils.SigmaByteWriter.DataInfo
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{ModQArithOpCompanion, SType, ModQArithOp}

case class ModQArithOpSerializer(override val opDesc: ModQArithOpCompanion, cons: (BigIntValue, BigIntValue) => BigIntValue)
  extends ValueSerializer[ModQArithOp] {
  val leftInfo: DataInfo[SValue] = opDesc.argInfos(0)
  val rightInfo: DataInfo[SValue] = opDesc.argInfos(1)

  override def serialize(obj: ModQArithOp, w: SigmaByteWriter): Unit = {
    w.putValue(obj.left, leftInfo)
      .putValue(obj.right, rightInfo)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val arg1 = r.getValue().asBigInt
    val arg2 = r.getValue().asBigInt
    cons(arg1, arg2)
  }
}
