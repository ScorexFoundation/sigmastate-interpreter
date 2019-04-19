package sigmastate.serialization.transformers

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import sigmastate.Operations.DeserializeRegisterInfo
import sigmastate.{ArgInfo, SType}
import sigmastate.Values.Value
import sigmastate.serialization.ValueSerializer
import ValueSerializer._
import sigmastate.Operations.DeserializeRegisterInfo._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.DeserializeRegister

case class DeserializeRegisterSerializer(cons: (RegisterId, SType, Option[Value[SType]]) => Value[SType])
  extends ValueSerializer[DeserializeRegister[SType]] {
  override def opDesc = DeserializeRegister

  override def serialize(obj: DeserializeRegister[SType], w: SigmaByteWriter): Unit = {
    w.put(obj.reg.number, idArg)
    w.putType(obj.tpe, ArgInfo("type", "expected type of the deserialized script"))
    opt(w, "default", obj.default)(_.putValue(_, defaultArg))
  }
  override def parse(r: SigmaByteReader): Value[SType] = {
    val registerId = ErgoBox.findRegisterByIndex(r.getByte()).get
    val tpe = r.getType()
    val dv = r.getOption(r.getValue())
    cons(registerId, tpe, dv)
  }

}
