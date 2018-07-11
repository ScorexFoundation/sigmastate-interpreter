package sigmastate.serialization

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.utils.{ByteReader, ByteWriterSigmaValues}
import sigmastate.{SType, TwoArgumentsOperation}


case class TwoArgumentsSerializer[LIV <: SType, RIV <: SType, OV <: Value[SType]]
(override val opCode: Byte, constructor: (Value[LIV], Value[RIV]) => Value[SType])
  extends ValueSerializer[OV] {

  override def serializeBody(obj: OV, w: ByteWriterSigmaValues): Unit = {
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
