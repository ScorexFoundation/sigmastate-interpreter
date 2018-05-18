package sigmastate.serialization.transformers

import sigmastate.{SBox, SType}
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer.{Position, Consumed}
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utxo.{ErgoBox, ExtractRegisterAs}

object ExtractRegisterAsSerializer extends ValueSerializer[ExtractRegisterAs[SType]] {
  override val opCode: OpCode = OpCodes.ExtractRegisterAs

  override def parseBody(bytes: Array[Byte], pos: Position): (ExtractRegisterAs[SType], Consumed) = {
    val (input, c1) = ValueSerializer.deserialize(bytes, pos)
    val registerId = ErgoBox.findRegisterByIndex(bytes(pos + c1)).get
    val (defaultValue, c2) = if (bytes(pos + c1 + 1) == 0) {
      None -> 1
    } else {
      val (dv, consumed) = ValueSerializer.deserialize(bytes, pos + c1 + 1)
      Some(dv) -> consumed
    }
    val tpeByte = bytes(pos + c1 + 1 + c2)
    val tpe = SType.allPredefTypes.filter(_.typeCode == tpeByte).head
    ExtractRegisterAs(input.asInstanceOf[Value[SBox.type]], registerId, defaultValue)(tpe) -> (pos + c1 + 1 + c2 + 1)
  }

  override def serializeBody(obj: ExtractRegisterAs[SType]): Array[Byte] = {
    val inputBytes = ValueSerializer.serialize(obj.input)
    val registerIdByte = obj.registerId.number
    val defaultValue = obj.default.fold(Array(0: Byte)) {
      ValueSerializer.serialize
    }
    val tpeByte = obj.tpe.typeCode
    inputBytes ++ Array(registerIdByte) ++ defaultValue ++ Array(tpeByte)
  }
}
