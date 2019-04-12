package sigmastate.serialization.transformers

import sigmastate.SType
import sigmastate.Values._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.DeserializeContext

case class DeserializeContextSerializer(cons: (Byte, SType) => Value[SType])
  extends ValueSerializer[DeserializeContext[SType]] {
  override def opDesc = DeserializeContext

  override def serialize(obj: DeserializeContext[SType], w: SigmaByteWriter): Unit =
    w.putType(obj.tpe)
      .put(obj.id)

  override def parse(r: SigmaByteReader): Value[SType] = {
    val tpe = r.getType()
    val id = r.getByte()
    cons(id, tpe)
  }
}
