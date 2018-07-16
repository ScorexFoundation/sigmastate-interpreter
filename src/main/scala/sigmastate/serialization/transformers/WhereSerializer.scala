package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.Extensions._
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.Where
import sigmastate.{SBoolean, SCollection, SType}

case class WhereSerializer(cons: (Value[SCollection[SType]], Byte, Value[SBoolean.type]) => Value[SCollection[SType]]) extends ValueSerializer[Where[SType]] {

  override val opCode: OpCode = OpCodes.WhereCode

  override def serializeBody(obj: Where[SType], w: ByteWriter): Unit =
    w.put(obj.id)
    .putValue(obj.input)
    .putValue(obj.condition)

  override def parseBody(r: ByteReader): Value[SCollection[SType]] = {
    val id = r.getByte()
    val input = r.getValue().asCollection[SType]
    val condition = r.getValue().asValue[SBoolean.type]
    cons(input, id, condition)
  }
}
