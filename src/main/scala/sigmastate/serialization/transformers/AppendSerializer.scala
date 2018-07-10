package sigmastate.serialization.transformers

import sigmastate.SType
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.Append

object AppendSerializer extends ValueSerializer[Append[SType]] {

  override val opCode: OpCode = OpCodes.AppendCode

  override def serializeBody(obj: Append[SType], w: ByteWriter): Unit =
    w.putValue(obj.input)
      .putValue(obj.col2)

  override def parseBody(r: ByteReader): Append[SType] = {
    val input = r.getValue().asCollection[SType]
    val col2 = r.getValue().asCollection[SType]
    Append(input, col2)
  }
}
