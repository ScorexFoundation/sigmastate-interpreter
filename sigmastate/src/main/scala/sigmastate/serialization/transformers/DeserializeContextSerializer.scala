package sigmastate.serialization.transformers

import sigmastate.{SType, ArgInfo}
import sigmastate.Values._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.DeserializeContext
import SigmaByteWriter._
import sigmastate.Operations.DeserializeContextInfo

case class DeserializeContextSerializer(cons: (Byte, SType) => Value[SType])
  extends ValueSerializer[DeserializeContext[SType]] {
  override def opDesc = DeserializeContext
  val typeInfo: DataInfo[SType] = ArgInfo("type", "expected type of the deserialized script")
  val idInfo: DataInfo[Byte] = DeserializeContextInfo.idArg

  override def serialize(obj: DeserializeContext[SType], w: SigmaByteWriter): Unit =
    w.putType(obj.tpe, typeInfo)
      .put(obj.id, idInfo)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val tpe = r.getType()
    val id = r.getByte()
    cons(id, tpe)
  }
}
