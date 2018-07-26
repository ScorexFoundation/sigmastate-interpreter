package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.Extensions._
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.Slice
import sigmastate.{SCollection, SInt, SType}

case class SliceSerializer(cons: (Value[SCollection[SType]], Value[SInt.type], Value[SInt.type]) => Value[SCollection[SType]])
  extends ValueSerializer[Slice[SType]] {

  override val opCode: OpCode = OpCodes.SliceCode

  override def serializeBody(obj: Slice[SType], w: ByteWriter): Unit =
    w.putValue(obj.input)
      .putValue(obj.from)
      .putValue(obj.until)

  override def parseBody(r: ByteReader): Value[SCollection[SType]] = {
    val input = r.getValue().asCollection[SType]
    val from = r.getValue().asValue[SInt.type]
    val until = r.getValue().asValue[SInt.type]
    cons(input, from, until)
  }
}
