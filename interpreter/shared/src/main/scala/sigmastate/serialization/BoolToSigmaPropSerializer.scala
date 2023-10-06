package sigmastate.serialization

import sigma.ast.{BoolToSigmaProp, _}
import sigma.ast.global._
import sigma.serialization.CoreByteWriter.DataInfo
import sigmastate.lang.Terms._
import sigmastate.utils.SigmaByteWriter._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class BoolToSigmaPropSerializer(cons: BoolValue => SigmaPropValue) extends ValueSerializer[BoolToSigmaProp] {
  import Operations.BoolToSigmaPropInfo._
  override def opDesc = BoolToSigmaProp
  val conditionInfo: DataInfo[SValue] = conditionArg

  def serialize(obj: BoolToSigmaProp, w: SigmaByteWriter): Unit = {
    w.putValue(obj.value, conditionInfo)
  }

  def parse(r: SigmaByteReader): Value[SType] = {
    val p = r.getValue().asBoolValue
    cons(p)
  }
}
