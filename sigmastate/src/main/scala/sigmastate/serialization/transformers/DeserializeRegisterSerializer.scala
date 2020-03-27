package sigmastate.serialization.transformers

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import sigmastate.{ArgInfo, SType}
import sigmastate.Values.{Value, SValue}
import sigmastate.serialization.ValueSerializer
import ValueSerializer._
import sigmastate.Operations.DeserializeRegisterInfo._
import sigmastate.utils.SigmaByteWriter.DataInfo
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.DeserializeRegister

case class DeserializeRegisterSerializer(cons: (RegisterId, SType, Option[Value[SType]]) => Value[SType])
  extends ValueSerializer[DeserializeRegister[SType]] {
  override def opDesc = DeserializeRegister
  val idInfo: DataInfo[Byte] = idArg
  val typeInfo: DataInfo[SType] = ArgInfo("type", "expected type of the deserialized script")
  val defaultInfo: DataInfo[SValue] = defaultArg

  override def serialize(obj: DeserializeRegister[SType], w: SigmaByteWriter): Unit = {
    w.put(obj.reg.number, idInfo)
    w.putType(obj.tpe, typeInfo)
    opt(w, "default", obj.default)(_.putValue(_, defaultInfo))
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val registerId = ErgoBox.findRegisterByIndex(r.getByte()).get
    val tpe = r.getType()
    val dv = r.getOption(r.getValue())
    cons(registerId, tpe, dv)
  }

}
