package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms.Apply
import sigmastate.utils.SigmaByteWriter._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ApplySerializer(cons: (Value[SType], IndexedSeq[Value[SType]]) => Value[SType])
  extends ValueSerializer[Apply] {
  import sigmastate.Operations.ApplyInfo._
  override def opDesc = Apply
  val funcInfo: DataInfo[SValue] = funcArg
  val argsInfo: DataInfo[Seq[SValue]] = argsArg
  val argsItemInfo: DataInfo[SValue] = valuesItemInfo(argsInfo)

  override def serialize(obj: Apply, w: SigmaByteWriter): Unit = {
    w.putValue(obj.func, funcInfo)
    w.putValues(obj.args, argsInfo, argsItemInfo)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val func = r.getValue()
    val args = r.getValues()
    cons(func, args)
  }
}
