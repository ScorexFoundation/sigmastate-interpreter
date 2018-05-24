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
    val buf = new ByteArrayBuilder()
    buf.appendValue(obj.input)
       .append     (obj.id)
       .appendValue(obj.zero)
       .append     (obj.accId)
       .appendValue(obj.foldOp)
    buf.toBytes
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (Fold[SType], Consumed) = {
    val buf = Serializer.start(bytes, pos)
    val input = buf.getValue
    val id = buf.get()
    val zero = buf.getValue
    val accId = buf.get()
    val foldOp = buf.getValue
    val totalConsumed = buf.position() - pos
    Fold(input.asInstanceOf[Value[SCollection[SType]]], id, zero, accId, foldOp) -> totalConsumed
  }
}
