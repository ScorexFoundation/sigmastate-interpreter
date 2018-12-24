package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Fold
import sigmastate.{SCollection, SFunc, SType}

case class FoldSerializer(cons: (Value[SCollection[SType]], Value[SType], Value[SFunc]) => Value[SType])
  extends ValueSerializer[Fold[SType, SType]] {
  override val opCode: OpCode = OpCodes.FoldCode

  override def serializeBody(obj: Fold[SType, SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input)
      .putValue(obj.zero)
      .putValue(obj.foldOp)

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val input  = r.getValue().asCollection[SType]
    val zero   = r.getValue()
    val foldOp = r.getValue().asFunc
    cons(input, zero, foldOp)
  }
}
