package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigma.util.Extensions._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Filter
import sigmastate.{SBoolean, SCollection, SType}

case class FilterSerializer(cons: (Value[SCollection[SType]], Byte, Value[SBoolean.type]) => Value[SCollection[SType]]) extends ValueSerializer[Filter[SType]] {

  override val opCode: OpCode = OpCodes.FilterCode

  override def serializeBody(obj: Filter[SType], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "Filter")

    SerializeLog.logPrintf(true, true, false, "id")
    w.put(obj.id)
    SerializeLog.logPrintf(false, true, false, "id")

    SerializeLog.logPrintf(true, true, false, "input")
    w.putValue(obj.input)
    SerializeLog.logPrintf(false, true, false, "input")

    SerializeLog.logPrintf(true, true, false, "condition")
    w.putValue(obj.condition)
    SerializeLog.logPrintf(false, true, false, "condition")

    SerializeLog.logPrintf(false, true, false, "Filter")
  }

  override def parseBody(r: SigmaByteReader): Value[SCollection[SType]] = {
    val id = r.getByte()
    val input = r.getValue().asCollection[SType]
    val condition = r.getValue().asValue[SBoolean.type]
    cons(input, id, condition)
  }
}
