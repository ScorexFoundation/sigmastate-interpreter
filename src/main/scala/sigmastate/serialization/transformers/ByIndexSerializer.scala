package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.Extensions._
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.ByIndex
import sigmastate.{SCollection, SInt, SType}

case class ByIndexSerializer(cons: (Value[SCollection[SType]], Value[SInt.type], Option[Value[SType]]) => Value[SType])
  extends ValueSerializer[ByIndex[SType]] {

  override val opCode: OpCode = OpCodes.ByIndexCode

  override def serializeBody(obj: ByIndex[SType], w: ByteWriter): Unit =
    w.putValue(obj.input)
      .putValue(obj.index)
      .putOption(obj.default)(_.putValue(_))

  override def parseBody(r: ByteReader): Value[SType] = {
    val input = r.getValue().asCollection[SType]
    val index = r.getValue().upcastTo(SInt)
    val default = r.getOption(r.getValue())
    cons(input, index, default)
  }

}
