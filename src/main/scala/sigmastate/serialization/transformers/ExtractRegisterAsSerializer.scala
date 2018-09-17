package sigmastate.serialization.transformers

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.Extensions._
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.ExtractRegisterAs
import sigmastate.{SBox, SOption, SType}

case class ExtractRegisterAsSerializer(cons: (Value[SBox.type], RegisterId, SOption[SType], Option[Value[SType]]) => Value[SType])
  extends ValueSerializer[ExtractRegisterAs[SType]] {
  override val opCode: OpCode = OpCodes.ExtractRegisterAs

  override def serializeBody(obj: ExtractRegisterAs[SType], w: ByteWriter): Unit =
    w.putValue(obj.input)
      .put(obj.registerId.number)
      .putOption(obj.default)((w, v) => w.putValue(v))
      .putType(obj.tpe.elemType)

  override def parseBody(r: ByteReader): Value[SType] = {
    val input = r.getValue()
    val regId = r.getByte()
    val register = ErgoBox.findRegisterByIndex(regId).get
    val defaultValue = r.getOption(r.getValue())
    val tpe = r.getType()
    cons(input.asInstanceOf[Value[SBox.type]], register, SOption(tpe), defaultValue)
  }
}
