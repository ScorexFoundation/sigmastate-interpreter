package sigmastate.serialization.transformers

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{ValueSerializer, OpCodes}
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteWriter, SigmaByteReader}
import sigmastate._

case class AtLeastSerializer(cons: (Value[SInt.type], Value[SCollection[SBoolean.type]]) => Value[SBoolean.type])
  extends ValueSerializer[AtLeast] {

  override val opCode: OpCode = OpCodes.AtLeastCode

  override def serialize(obj: AtLeast, w: SigmaByteWriter): Unit =
    w.putValue(obj.bound)
      .putValue(obj.input)

  override def parse(r: SigmaByteReader): Value[SBoolean.type] = {
    val bound = r.getValue().asIntValue
    val input = r.getValue().asCollection[SBoolean.type]
    cons(bound, input)
  }
}
