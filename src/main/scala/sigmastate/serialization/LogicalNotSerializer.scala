package sigmastate.serialization

import sigmastate.LogicalNot
import sigmastate.Values.BoolValue
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class LogicalNotSerializer(cons: BoolValue => BoolValue)
  extends ValueSerializer[LogicalNot] {
  override def opDesc = LogicalNot
  override def serialize(obj: LogicalNot, w: SigmaByteWriter): Unit = w.putValue(obj.input)
  override def parse(r: SigmaByteReader): BoolValue = cons(r.getValue().asBoolValue)
}
