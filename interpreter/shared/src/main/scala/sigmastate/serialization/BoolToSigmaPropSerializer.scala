package sigmastate.serialization

import sigma.ast.SType
import sigma.serialization.CoreByteWriter.DataInfo
import sigmastate.Values.{BoolValue, SValue, SigmaPropValue}
import sigmastate.lang.Terms._
import sigmastate.utils.SigmaByteWriter._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{BoolToSigmaProp, Values}

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
