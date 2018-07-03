package sigmastate.serialization.transformers

import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.Slice
import sigmastate.{SInt, SType}

object SliceSerializer extends ValueSerializer[Slice[SType]] {

  override val opCode: OpCode = OpCodes.SliceCode

  override def parseBody(r: ByteReader): Slice[SType] = {
    val input = r.getValue().asCollection[SType]
    val from = r.getValue().asValue[SInt.type]
    val until = r.getValue().asValue[SInt.type]
    Slice(input, from, until)
  }

  override def serializeBody(obj: Slice[SType], w: ByteWriter): Unit =
    w.putValue(obj.input)
      .putValue(obj.from)
      .putValue(obj.until)
}
