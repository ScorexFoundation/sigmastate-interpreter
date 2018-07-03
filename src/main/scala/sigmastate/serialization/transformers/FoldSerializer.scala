package sigmastate.serialization.transformers

import sigmastate.SType
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.Fold

object FoldSerializer extends ValueSerializer[Fold[SType]] {
  override val opCode: OpCode = OpCodes.FoldCode

  override def parseBody(r: ByteReader): Fold[SType] = {
    val input  = r.getValue()
    val id     = r.getByte()
    val zero   = r.getValue()
    val accId  = r.getByte()
    val foldOp = r.getValue()
    Fold(input.asCollection[SType], id, zero, accId, foldOp)
  }

  override def serializeBody(obj: Fold[SType], w: ByteWriter): Unit =
    w.putValue(obj.input)
      .put     (obj.id)
      .putValue(obj.zero)
      .put     (obj.accId)
      .putValue(obj.foldOp)
}
