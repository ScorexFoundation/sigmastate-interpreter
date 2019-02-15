package sigmastate.serialization.transformers

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigma.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.DeserializeContext

case class DeserializeContextSerializer(cons: (Byte, SType) => Value[SType])
  extends ValueSerializer[DeserializeContext[SType]] {

  override val opCode: OpCode = OpCodes.DeserializeContextCode

  override def serializeBody(obj: DeserializeContext[SType], w: SigmaByteWriter): Unit =
    w.putType(obj.tpe)
      .put(obj.id)

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val tpe = r.getType()
    val id = r.getByte()
    cons(id, tpe)
  }
}
