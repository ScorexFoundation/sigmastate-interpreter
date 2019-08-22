package sigmastate.serialization.transformers

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import sigmastate.Values.Value
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.ExtractRegisterAs
import sigmastate.{SBox, SOption, SType, ArgInfo}

case class ExtractRegisterAsSerializer(cons: (Value[SBox.type], RegisterId, SOption[SType]) => Value[SType])
  extends ValueSerializer[ExtractRegisterAs[SType]] {
  import sigmastate.Operations.ExtractRegisterAsInfo._
  override def opDesc = ExtractRegisterAs

  override def serialize(obj: ExtractRegisterAs[SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, thisArg)
      .put(obj.registerId.number, regIdArg)
      .putType(obj.tpe.elemType, ArgInfo("type", "expected type of the value in register"))

  override def parse(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue()
    val regId = r.getByte()
    val register = ErgoBox.findRegisterByIndex(regId).get
    val tpe = r.getType()
    cons(input.asInstanceOf[Value[SBox.type]], register, SOption(tpe))
  }
}
