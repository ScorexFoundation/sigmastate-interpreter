package sigmastate.serialization.transformers

import org.ergoplatform.ErgoBox
import sigmastate.SType
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.DeserializeRegister
import sigmastate.utils.Extensions._

object DeserializeRegisterSerializer extends ValueSerializer[DeserializeRegister[SType]] {

  override val opCode: OpCode = OpCodes.DeserializeRegisterCode

  override def serializeBody(obj: DeserializeRegister[SType], w: ByteWriter): Unit =
    w.put(obj.reg.number)
      .putType(obj.tpe)
      .putOption(obj.default)(_.putValue(_))

  override def parseBody(r: ByteReader): DeserializeRegister[SType] = {
    val registerId = ErgoBox.findRegisterByIndex(r.getByte()).get
    val tpe = r.getType()
    val dv = r.getOption(r.getValue())
    DeserializationSigmaBuilder.mkDeserializeRegister(registerId, tpe, dv)
      .asInstanceOf[DeserializeRegister[SType]]
  }

}
