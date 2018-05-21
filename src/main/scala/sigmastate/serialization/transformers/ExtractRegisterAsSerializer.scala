package sigmastate.serialization.transformers

import sigmastate.{SBox, SType}
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer.{Position, Consumed}
import sigmastate.serialization.{ValueSerializer, OpCodes, STypeSerializer}
import sigmastate.utxo.{ExtractRegisterAs, ErgoBox}

object ExtractRegisterAsSerializer extends ValueSerializer[ExtractRegisterAs[SType]] {
  override val opCode: OpCode = OpCodes.ExtractRegisterAs

  override def serializeBody(obj: ExtractRegisterAs[SType]): Array[Byte] = {
    val inputBytes = ValueSerializer.serialize(obj.input)
    val registerIdByte = obj.registerId.number
    val defaultValue = obj.default.fold(Array(0: Byte))(ValueSerializer.serialize)
    val tpeBytes = STypeSerializer.serialize(obj.tpe)
    inputBytes ++ Array(registerIdByte) ++ defaultValue ++ tpeBytes
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (ExtractRegisterAs[SType], Consumed) = {
    val (input, c1) = ValueSerializer.deserialize(bytes, pos)

    val idPos = pos + c1
    val registerId = ErgoBox.findRegisterByIndex(bytes(idPos)).get

    val defaultPos = idPos + 1
    val noDefault = bytes(defaultPos) == 0
    val (defaultValue, c2) = if (noDefault) {
      None -> 1
    } else {
      val (dv, consumed) = ValueSerializer.deserialize(bytes, defaultPos)
      Some(dv) -> consumed
    }

    val typePos = defaultPos + c2
    val (tpe, c3) = STypeSerializer.deserialize(bytes, typePos)
    val parsed = ExtractRegisterAs(input.asInstanceOf[Value[SBox.type]], registerId, defaultValue)(tpe)
    val consumed =  (typePos + c3) - pos
    parsed -> consumed
  }
}
