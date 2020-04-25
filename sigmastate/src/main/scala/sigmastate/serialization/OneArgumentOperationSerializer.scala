package sigmastate.serialization

import sigmastate.Values.{Value, SValue}
import sigmastate.lang.Terms._
import sigmastate.utils.SigmaByteWriter.DataInfo
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{OneArgumentOperation, OneArgumentOperationCompanion, SType}

case class OneArgumentOperationSerializer[T <: SType](opDesc: OneArgumentOperationCompanion, cons: Value[T] => SValue)
  extends ValueSerializer[OneArgumentOperation[T, SType]] {
  val objInfo: DataInfo[SValue] = opDesc.argInfos(0)

  override def serialize(obj: OneArgumentOperation[T, SType], w: SigmaByteWriter): Unit =
    w.putValue(obj.input, objInfo)
    
  override def parse(r: SigmaByteReader): SValue =
    cons(r.getValue().asValue[T])
}
