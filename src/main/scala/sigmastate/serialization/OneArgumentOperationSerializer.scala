package sigmastate.serialization

import sigmastate.Values.{Value, ValueCompanion, SValue}
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{OneArgumentOperation, SType}

case class OneArgumentOperationSerializer[T <: SType](opDesc: ValueCompanion, cons: Value[T] => SValue)
  extends ValueSerializer[OneArgumentOperation[T, SType]] {
  override def serialize(obj: OneArgumentOperation[T, SType], w: SigmaByteWriter): Unit = w.putValue(obj.input)
  override def parse(r: SigmaByteReader): SValue = cons(r.getValue().asValue[T])
}
