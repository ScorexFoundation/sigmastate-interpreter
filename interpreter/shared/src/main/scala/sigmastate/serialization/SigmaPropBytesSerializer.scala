package sigmastate.serialization

import sigma.ast.{SType, Value}
import sigma.serialization.CoreByteWriter.DataInfo
import sigma.ast.global.SValue
import sigmastate.lang.Terms._
import sigmastate.utils.SigmaByteWriter._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.SigmaPropBytes

object SigmaPropBytesSerializer extends ValueSerializer[SigmaPropBytes] {
  import sigma.ast.Operations.SigmaPropBytesInfo._
  override def opDesc = SigmaPropBytes
  val thisInfo: DataInfo[SValue] = thisArg

  def serialize(obj: SigmaPropBytes, w: SigmaByteWriter): Unit = {
    w.putValue(obj.input, thisInfo)
  }

  def parse(r: SigmaByteReader): Value[SType] = {
    val p = r.getValue().asSigmaProp
    SigmaPropBytes(p)
  }
}
