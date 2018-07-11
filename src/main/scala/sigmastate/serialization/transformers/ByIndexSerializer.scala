package sigmastate.serialization.transformers

import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{ByteReader, ByteWriterSigmaValues}
import sigmastate.utxo.ByIndex
import sigmastate.{SInt, SType}

object ByIndexSerializer extends ValueSerializer[ByIndex[SType]] {

  override val opCode: OpCode = OpCodes.ByIndexCode

  override def serializeBody(obj: ByIndex[SType], w: ByteWriterSigmaValues): Unit =
    w.putValue(obj.input)
      .putValue(obj.index)
      .putOption(obj.default)(_.putValue(_))

  override def parseBody(r: ByteReader): ByIndex[SType] = {
    val input = r.getValue().asCollection[SType]
    val index = r.getValue().upcastTo(SInt)
    val default = r.getOption(r.getValue())
    ByIndex(input, index, default)
  }

}
