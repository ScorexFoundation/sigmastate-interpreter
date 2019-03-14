package sigmastate.serialization.transformers

import sigmastate.Values.{SigmaPropValue, Value}
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

case class AtLeastSerializer(cons: (Value[SInt.type], Value[SCollection[SSigmaProp.type]]) => SigmaPropValue)
  extends ValueSerializer[AtLeast] {

  override val opCode: OpCode = OpCodes.AtLeastCode

  override def serializeBody(obj: AtLeast, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "AtLeast")

    SerializeLog.logPrintf(true, true, false, "bound")
    w.putValue(obj.bound)
    SerializeLog.logPrintf(false, true, false, "bound")

    SerializeLog.logPrintf(true, true, false, "input")
    w.putValue(obj.input)
    SerializeLog.logPrintf(false, true, false, "input")

    SerializeLog.logPrintf(false, true, false, "AtLeast")
  }

  override def parseBody(r: SigmaByteReader): SigmaPropValue = {
    val bound = r.getValue().asIntValue
    val input = r.getValue().asCollection[SSigmaProp.type]
    cons(bound, input)
  }
}
