package sigmastate.serialization.transformers

import org.ergoplatform.ErgoBox
import sigmastate.SType
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer._
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utxo.DeserializeRegister

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
    val registerId = ErgoBox.findRegisterByIndex(bytes(pos)).get
    val tpeByte = bytes(pos + 1)
    val tpe = SType.allPredefTypes.filter(_.typeCode == tpeByte).head
    if (bytes(pos + 2) == 0) {
      (DeserializeRegister(registerId, None)(tpe), 3)
    } else {
      val (dv, consumed) = ValueSerializer.deserialize(bytes, pos + 2)
      (DeserializeRegister(registerId, Some(dv))(tpe), consumed + 2)
    }
  }

}
