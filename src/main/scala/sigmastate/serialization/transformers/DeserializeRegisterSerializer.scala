package sigmastate.serialization.transformers

import sigmastate.SType
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.ValueSerializer._
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utxo.{DeserializeRegister, ErgoBox}

object DeserializeRegisterSerializer extends ValueSerializer[DeserializeRegister[SType]] {

  override val opCode: OpCode = OpCodes.DeserializeRegisterCode

  override def serializeBody(obj: DeserializeRegister[SType]): Array[Byte] = {
    val registerIdByte = obj.reg.number
    val defaultValue = obj.default.fold(Array(0: Byte)) {
      ValueSerializer.serialize
    }
    val tpeByte = obj.tpe.typeCode

    Array(registerIdByte, tpeByte) ++ defaultValue
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (DeserializeRegister[SType], Consumed) = {
    val registedId = ErgoBox.findRegisterByIndex(bytes(pos)).get
    val tpeByte = bytes(pos + 1)
    val tpe = SType.allPredefTypes.filter(_.typeCode == tpeByte).head
    if (bytes(pos + 2) == 0) {
      (DeserializeRegister(registedId, None)(tpe), 3)
    } else {
      val (dv, consumed) = ValueSerializer.deserialize(bytes, pos + 2)
      (DeserializeRegister(registedId, Some(dv))(tpe), consumed + 2)
    }
  }

}
