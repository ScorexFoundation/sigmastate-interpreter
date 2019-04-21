package sigmastate.serialization

import sigmastate.Values.{Value, ValueCompanion}
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{TwoArgumentsOperation, SType, TwoArgumentOperationCompanion}

case class TwoArgumentsSerializer[LIV <: SType, RIV <: SType, OV <: Value[SType]]
(override val opDesc: TwoArgumentOperationCompanion, constructor: (Value[LIV], Value[RIV]) => Value[SType])
  extends ValueSerializer[OV] {

  override def serialize(obj: OV, w: SigmaByteWriter): Unit = {
    val typedOp = obj.asInstanceOf[TwoArgumentsOperation[LIV, RIV, LIV]]
    w.putValue(typedOp.left, opDesc.argInfos(0))
      .putValue(typedOp.right, opDesc.argInfos(1))
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val arg1 = r.getValue().asValue[LIV]
    val arg2 = r.getValue().asValue[RIV]
    constructor(arg1, arg2)
  }
}
