package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms.Apply
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ApplySerializer(cons: (Value[SType], IndexedSeq[Value[SType]]) => Value[SType])
  extends ValueSerializer[Apply] {
  import sigmastate.Operations.ApplyInfo._
  override def opDesc = Apply

  override def serialize(obj: Apply, w: SigmaByteWriter): Unit = {
    w.putValue(obj.func, funcArg)
    w.putValues(obj.args, argsArg)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val func = r.getValue()
    val args = r.getValues()
    cons(func, args)
  }
}
