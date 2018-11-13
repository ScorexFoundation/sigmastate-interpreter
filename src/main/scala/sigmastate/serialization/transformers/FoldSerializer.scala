package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Fold
import sigmastate.{SCollection, SType}

case class FoldSerializer(cons: (Value[SCollection[SType]], Byte, Value[SType], Byte, Value[SType]) => Value[SType])
  extends ValueSerializer[Fold[SType, SType]] {
  override val opCode: OpCode = OpCodes.FoldCode

  override def serializeBody(obj: Fold[SType, SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input)
      .put     (obj.id)
      .putValue(obj.zero)
      .put     (obj.accId)
      .putValue(obj.foldOp)

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val input  = r.getValue()
    val id     = r.getByte()
    val zero   = r.getValue()
    val accId  = r.getByte()
    val foldOp = r.getValue()
    cons(input.asCollection[SType], id, zero, accId, foldOp)
  }
}
