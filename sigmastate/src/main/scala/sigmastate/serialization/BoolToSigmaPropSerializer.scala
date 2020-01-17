package sigmastate.serialization

import sigmastate.Values.{BoolValue, SigmaPropValue, SValue}
import sigmastate.lang.Terms._
import sigmastate.utils.SigmaByteWriter.DataInfo
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{BoolToSigmaProp, SType, Values}

case class BoolToSigmaPropSerializer(cons: BoolValue => SigmaPropValue) extends ValueSerializer[BoolToSigmaProp] {
  import sigmastate.Operations.BoolToSigmaPropInfo._
  override def opDesc = BoolToSigmaProp
  val conditionInfo: DataInfo[SValue] = conditionArg

  def serialize(obj: BoolToSigmaProp, w: SigmaByteWriter): Unit = {
    w.putValue(obj.value, conditionInfo)
  }

  def parse(r: SigmaByteReader): Values.Value[SType] = {
    val p = r.getValue().asBoolValue
    cons(p)
  }
}
