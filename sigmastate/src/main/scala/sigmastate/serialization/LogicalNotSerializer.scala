package sigmastate.serialization

import sigmastate.LogicalNot
import sigmastate.Operations.LogicalNotInfo.inputArg
import sigmastate.Values.{BoolValue, SValue}
import sigmastate.lang.Terms._
import sigmastate.utils.SigmaByteWriter.DataInfo
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class LogicalNotSerializer(cons: BoolValue => BoolValue)
  extends ValueSerializer[LogicalNot] {
  override def opDesc = LogicalNot
  val inputInfo: DataInfo[SValue] = inputArg

  override def serialize(obj: LogicalNot, w: SigmaByteWriter): Unit =
    w.putValue(obj.input, inputInfo)

  override def parse(r: SigmaByteReader): BoolValue =
    cons(r.getValue().asBoolValue)
}
