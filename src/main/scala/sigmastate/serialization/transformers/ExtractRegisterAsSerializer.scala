package sigmastate.serialization.transformers

import org.ergoplatform.ErgoBox
import sigmastate.{SBox, SType}
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer.{Position, Consumed}
import sigmastate.serialization.{ValueSerializer, OpCodes, Serializer}
import sigmastate.utxo.ExtractRegisterAs

object ExtractRegisterAsSerializer extends ValueSerializer[ExtractRegisterAs[SType]] {
  override val opCode: OpCode = OpCodes.ExtractRegisterAs

  override def serializeBody(obj: ExtractRegisterAs[SType]): Array[Byte] = {
    val w = Serializer.startWriter()
        .putValue(obj.input)
        .put(obj.registerId.number)
        .putOption(obj.default)((w, v) => w.putValue(v))
        .putType(obj.tpe)
    w.toBytes
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (ExtractRegisterAs[SType], Consumed) = {
    val r = Serializer.startReader(bytes, pos)
    val input = r.getValue()
    val regId = r.getByte()
    val register = ErgoBox.findRegisterByIndex(regId).get
    val defaultValue = r.getOption(r.getValue())
    val tpe = r.getType()
    val parsed = ExtractRegisterAs(input.asInstanceOf[Value[SBox.type]], register, defaultValue)(tpe)
    parsed -> r.consumed
  }
}
