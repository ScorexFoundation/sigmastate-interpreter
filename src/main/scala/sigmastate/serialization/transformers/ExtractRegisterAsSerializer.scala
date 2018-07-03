package sigmastate.serialization.transformers

import org.ergoplatform.ErgoBox
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.ExtractRegisterAs
import sigmastate.{SBox, SType}

object ExtractRegisterAsSerializer extends ValueSerializer[ExtractRegisterAs[SType]] {
  override val opCode: OpCode = OpCodes.ExtractRegisterAs

  override def parseBody(r: ByteReader): ExtractRegisterAs[SType] = {
    val input = r.getValue()
    val regId = r.getByte()
    val register = ErgoBox.findRegisterByIndex(regId).get
    val defaultValue = r.getOption(r.getValue())
    val tpe = r.getType()
    ExtractRegisterAs(input.asInstanceOf[Value[SBox.type]], register, defaultValue)(tpe)
  }

  override def serializeBody(obj: ExtractRegisterAs[SType], w: ByteWriter): Unit =
    w.putValue(obj.input)
      .put(obj.registerId.number)
      .putOption(obj.default)((w, v) => w.putValue(v))
      .putType(obj.tpe)

}
