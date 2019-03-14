package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms.Apply
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

case class ApplySerializer(cons: (Value[SType], IndexedSeq[Value[SType]]) => Value[SType])
  extends ValueSerializer[Apply] {

  override val opCode: OpCode = FuncApplyCode

  override def serializeBody(obj: Apply, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "Apply")

    SerializeLog.logPrintf(true, true, false, "func")
    w.putValue(obj.func)
    SerializeLog.logPrintf(false, true, false, "func")

    SerializeLog.logPrintf(true, true, false, "args")
    w.putValues(obj.args)
    SerializeLog.logPrintf(false, true, false, "args")

    SerializeLog.logPrintf(false, true, false, "Apply")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val func = r.getValue()
    val args = r.getValues()
    cons(func, args)
  }
}
