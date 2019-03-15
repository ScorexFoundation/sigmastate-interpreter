package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Append
import sigmastate.{SCollection, SType}

case class AppendSerializer(cons: (Value[SCollection[SType]], Value[SCollection[SType]]) => Value[SCollection[SType]])
  extends ValueSerializer[Append[SType]] {

  override val opCode: OpCode = OpCodes.AppendCode

  override def serializeBody(obj: Append[SType], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "Append")

    SerializeLog.logPrintf(true, true, false, "input")
    w.putValue(obj.input)
    SerializeLog.logPrintf(false, true, false, "input")

    SerializeLog.logPrintf(true, true, false, "col2")
    w.putValue(obj.col2)
    SerializeLog.logPrintf(false, true, false, "col2")

    SerializeLog.logPrintf(false, true, false, "Append")
  }

  override def parseBody(r: SigmaByteReader): Value[SCollection[SType]] = {
    val input = r.getValue().asCollection[SType]
    val col2 = r.getValue().asCollection[SType]
    cons(input, col2)
  }
}
