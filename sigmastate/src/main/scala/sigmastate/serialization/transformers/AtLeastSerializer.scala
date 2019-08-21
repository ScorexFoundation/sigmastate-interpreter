package sigmastate.serialization.transformers

import sigmastate.Operations.AtLeastInfo
import sigmastate.Values.{Value, SigmaPropValue}
import sigmastate.lang.Terms._
import sigmastate._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class AtLeastSerializer(cons: (Value[SInt.type], Value[SCollection[SSigmaProp.type]]) => SigmaPropValue)
  extends ValueSerializer[AtLeast] {
  override def opDesc = AtLeast

  override def serialize(obj: AtLeast, w: SigmaByteWriter): Unit =
    w.putValue(obj.bound, AtLeastInfo.boundArg)
      .putValue(obj.input, AtLeastInfo.childrenArg)

  override def parse(r: SigmaByteReader): SigmaPropValue = {
    val bound = r.getValue().asIntValue
    val input = r.getValue().asCollection[SSigmaProp.type]
    cons(bound, input)
  }
}
