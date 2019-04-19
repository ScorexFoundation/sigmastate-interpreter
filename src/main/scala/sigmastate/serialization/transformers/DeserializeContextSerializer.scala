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

  override def serialize(obj: DeserializeContext[SType], w: SigmaByteWriter): Unit =
    w.putType(obj.tpe, ArgInfo("type", "expected type of the deserialized script"))
      .put(obj.id, DeserializeContextInfo.idArg)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val tpe = r.getType()
    val id = r.getByte()
    cons(id, tpe)
  }
}
