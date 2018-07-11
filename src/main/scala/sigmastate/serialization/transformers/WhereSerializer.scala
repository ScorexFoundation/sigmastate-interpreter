package sigmastate.serialization.transformers

import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReaderSigmaValues, ByteWriterSigmaValues}
import sigmastate.utxo.Where
import sigmastate.{SBoolean, SType}

object WhereSerializer extends ValueSerializer[Where[SType]] {

  override val opCode: OpCode = OpCodes.WhereCode

  override def serializeBody(obj: Where[SType], w: ByteWriterSigmaValues): Unit =
    w.put(obj.id)
    .putValue(obj.input)
    .putValue(obj.condition)

  override def parseBody(r: ByteReaderSigmaValues): Where[SType] = {
    val id = r.getByte()
    val input = r.getValue().asCollection[SType]
    val condition = r.getValue().asValue[SBoolean.type]
    Where(input, id, condition)
  }
}
