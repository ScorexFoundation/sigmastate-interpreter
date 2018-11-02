package sigmastate.serialization

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.utils.{ByteWriter, ByteReader}
import sigmastate.{TwoArgumentsOperation, SType, SBigInt}
import sigmastate.utils.Extensions._
import OpCodes._
import sigmastate.utxo.CostTable._

case class TwoArgumentsSerializer[LIV <: SType, RIV <: SType, OV <: Value[SType]]
(override val opCode: Byte, constructor: (Value[LIV], Value[RIV]) => Value[SType])
  extends ValueSerializer[OV] {

  override def serializeBody(obj: OV, w: ByteWriter): Unit = {
    val typedOp = obj.asInstanceOf[TwoArgumentsOperation[LIV, RIV, LIV]]
    w.putValue(typedOp.left)
      .putValue(typedOp.right)
  }

  override def parseBody(r: ByteReader): Value[SType] = {
    val arg1 = r.getValue().asValue[LIV]
    val arg2 = r.getValue().asValue[RIV]
    constructor(arg1, arg2)
  }
}
