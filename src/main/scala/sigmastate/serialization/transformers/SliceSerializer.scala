package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Slice
import sigmastate.{SCollection, SInt, SType}

case class SliceSerializer(cons: (Value[SCollection[SType]], Value[SInt.type], Value[SInt.type]) => Value[SCollection[SType]])
  extends ValueSerializer[Slice[SType]] {

  override val opCode: OpCode = OpCodes.SliceCode

  override def serializeBody(obj: Slice[SType], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "Slice")

    SerializeLog.logPrintf(true, true, false, "input")
    w.putValue(obj.input)
    SerializeLog.logPrintf(false, true, false, "input")

    SerializeLog.logPrintf(true, true, false, "from")
    w.putValue(obj.from)
    SerializeLog.logPrintf(false, true, false, "from")

    SerializeLog.logPrintf(true, true, false, "until")
    w.putValue(obj.until)
    SerializeLog.logPrintf(false, true, false, "until")

    SerializeLog.logPrintf(false, true, false, "Slice")
  }

  override def parseBody(r: SigmaByteReader): Value[SCollection[SType]] = {
    val input = r.getValue().asCollection[SType]
    val from = r.getValue().asValue[SInt.type]
    val until = r.getValue().asValue[SInt.type]
    cons(input, from, until)
  }
}
