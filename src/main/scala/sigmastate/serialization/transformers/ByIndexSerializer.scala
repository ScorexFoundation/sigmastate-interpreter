package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigma.util.Extensions._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.ByIndex
import sigmastate.{SCollection, SInt, SType}

case class ByIndexSerializer(cons: (Value[SCollection[SType]], Value[SInt.type], Option[Value[SType]]) => Value[SType])
  extends ValueSerializer[ByIndex[SType]] {

  override val opCode: OpCode = OpCodes.ByIndexCode

  override def serializeBody(obj: ByIndex[SType], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "ByIndex")

    SerializeLog.logPrintf(true, true, false, "input")
    w.putValue(obj.input)
    SerializeLog.logPrintf(false, true, false, "input")

    SerializeLog.logPrintf(true, true, false, "index")
    w.putValue(obj.index)
    SerializeLog.logPrintf(false, true, false, "index")

    SerializeLog.logPrintf(true, true, false, "default")
    w.putOption(obj.default)(_.putValue(_))
    SerializeLog.logPrintf(false, true, false, "default")

    SerializeLog.logPrintf(false, true, false, "ByIndex")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue().asCollection[SType]
    val index = r.getValue().upcastTo(SInt)
    val default = r.getOption(r.getValue())
    cons(input, index, default)
  }

}
