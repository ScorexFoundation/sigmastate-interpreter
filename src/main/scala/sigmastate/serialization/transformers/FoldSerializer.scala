package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer.{Position, Consumed}
import sigmastate.serialization.{ValueSerializer, OpCodes, Serializer}
import sigmastate.utils.ByteArrayBuilder
import sigmastate.utils.Extensions._
import sigmastate.utxo.Fold
import sigmastate.{SCollection, SType}

object FoldSerializer extends ValueSerializer[Fold[SType]] {
  override val opCode: OpCode = OpCodes.FoldCode

  override def serializeBody(obj: Fold[SType]): Array[Byte] = {
    val w = Serializer.startWriter()
       .putValue(obj.input)
       .put     (obj.id)
       .putValue(obj.zero)
       .put     (obj.accId)
       .putValue(obj.foldOp)
    w.toBytes
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (Fold[SType], Consumed) = {
    val r = Serializer.startReader(bytes, pos)
    val input  = r.getValue
    val id     = r.getByte()
    val zero   = r.getValue
    val accId  = r.getByte()
    val foldOp = r.getValue
    val res = Fold(input.asInstanceOf[Value[SCollection[SType]]], id, zero, accId, foldOp)
    res -> r.consumed
  }
}
