package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms.Apply
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ApplySerializer(cons: (Value[SType], IndexedSeq[Value[SType]]) => Value[SType])
  extends ValueSerializer[Apply] {

  override val opCode: OpCode = FuncApplyCode

  override def serialize(obj: Apply, w: SigmaByteWriter): Unit = {
    w.putValue(obj.func)
    w.putValues(obj.args)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val func = r.getValue()
    val args = r.getValues()
    cons(func, args)
  }
}
