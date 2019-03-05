package sigmastate.serialization

import sigmastate.Values.{SValue, Value}
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{OneArgumentOperation, SNumericType, SType}

case class OneArgumentOperationSerializer[T <: SType](opCode: OpCode, cons: Value[T] => SValue)
  extends ValueSerializer[OneArgumentOperation[T, SType]] {
  override def serialize(obj: OneArgumentOperation[T, SType], w: SigmaByteWriter): Unit = w.putValue(obj.input)
  override def parse(r: SigmaByteReader): SValue = cons(r.getValue().asValue[T])
}
