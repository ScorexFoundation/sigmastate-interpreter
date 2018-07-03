package sigmastate.serialization.transformers

import org.ergoplatform.ErgoBox
import sigmastate.SType
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.DeserializeRegister

object DeserializeRegisterSerializer extends ValueSerializer[DeserializeRegister[SType]] {

  override val opCode: OpCode = OpCodes.DeserializeRegisterCode

  override def parseBody(r: ByteReader): DeserializeRegister[SType] = {
    val registerId = ErgoBox.findRegisterByIndex(r.getByte()).get
    val tpeByte = r.getByte()
    val tpe = SType.allPredefTypes.filter(_.typeCode == tpeByte).head
    val dv = r.getOption(r.getValue())
    DeserializeRegister(registerId, tpe, dv)
  }

  override def serializeBody(obj: DeserializeRegister[SType], w: ByteWriter): Unit =
    w.put(obj.reg.number)
      .put(obj.tpe.typeCode)
      .putOption(obj.default)(_.putValue(_))

}
