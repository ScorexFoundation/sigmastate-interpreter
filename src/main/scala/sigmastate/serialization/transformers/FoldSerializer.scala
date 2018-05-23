package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer.{Position, Consumed}
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utxo.Fold
import sigmastate.{SCollection, SType}

object FoldSerializer extends ValueSerializer[Fold[SType]] {
  override val opCode: OpCode = OpCodes.FoldCode

  override def parseBody(bytes: Array[Byte], pos: Position): (Fold[SType], Consumed) = {
    val (input, c1) = ValueSerializer.deserialize(bytes, pos)
    val id = bytes(pos + c1)
    val (zero, c2) = ValueSerializer.deserialize(bytes, pos + c1 + 1)
    val accId = bytes(pos + c1 + 1 + c2)
    val (foldOp, c3) = ValueSerializer.deserialize(bytes, pos + c1 + 1 + c2 + 1)
    val tpeTypeCode = bytes(pos + c1 + 1 + c2 + 1 + c3)
    val tpe = SType.allPredefTypes.filter(_.typeCode == tpeTypeCode).head
    val totalConsumed = pos + c1 + 1 + c2 + 1 + c3 + 1
    Fold(input.asInstanceOf[Value[SCollection[SType]]], id, zero, accId, foldOp) -> totalConsumed
  }

  override def serializeBody(obj: Fold[SType]): Array[Byte] = {
    val inputBytes = ValueSerializer.serialize(obj.input)
    val idByte = obj.id
    val zeroValueBytes = ValueSerializer.serialize(obj.zero)
    val accIdByte = obj.accId
    val foldOpBytes = ValueSerializer.serialize(obj.foldOp)
    val tpeByte = obj.tpe.typeCode
    inputBytes ++ Array(idByte) ++ zeroValueBytes ++ Array(accIdByte) ++ foldOpBytes ++ Array(tpeByte)
  }
}
