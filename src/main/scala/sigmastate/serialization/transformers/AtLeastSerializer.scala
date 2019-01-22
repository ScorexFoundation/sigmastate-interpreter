package sigmastate.serialization.transformers

import sigmastate.Values.{SigmaPropValue, Value}
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{ValueSerializer, OpCodes}
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteWriter, SigmaByteReader}
import sigmastate._
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import scorex.util.Extensions._

case class AtLeastSerializer(cons: (Value[SInt.type], Value[SCollection[SSigmaProp.type]]) => SigmaPropValue)
  extends ValueSerializer[AtLeast] {

  override val opCode: OpCode = OpCodes.AtLeastCode

  override def serialize(obj: AtLeast, w: SigmaByteWriter): Unit =
    w.putValue(obj.bound)
      .putValue(obj.input)

  override def parse(r: SigmaByteReader): SigmaPropValue = {
    val bound = r.getValue().asIntValue
    val input = r.getValue().asCollection[SSigmaProp.type]
    cons(bound, input)
  }
}
