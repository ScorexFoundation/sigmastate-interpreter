package sigmastate.serialization

import sigmastate.Values.{BoolValue, SigmaPropValue}
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{BoolToSigmaProp, SType, Values, LogicalNot}

case class BoolToSigmaPropSerializer(cons: BoolValue => SigmaPropValue) extends ValueSerializer[BoolToSigmaProp] {
  import sigmastate.Operations.BoolToSigmaPropInfo._
  override def opDesc = BoolToSigmaProp

  def serialize(obj: BoolToSigmaProp, w: SigmaByteWriter): Unit = {
    w.putValue(obj.value, conditionArg)
  }

  def parse(r: SigmaByteReader): Values.Value[SType] = {
    val p = r.getValue().asBoolValue
    cons(p)
  }
}
