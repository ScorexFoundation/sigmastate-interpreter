package sigmastate.serialization

import sigmastate.Values.SValue
import sigmastate.{Values, SType}
import sigmastate.lang.Terms._
import sigmastate.utils.SigmaByteWriter.DataInfo
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.SigmaPropBytes

object SigmaPropBytesSerializer extends ValueSerializer[SigmaPropBytes] {
  import sigmastate.Operations.SigmaPropBytesInfo._
  override def opDesc = SigmaPropBytes
  val thisInfo: DataInfo[SValue] = thisArg

  def serialize(obj: SigmaPropBytes, w: SigmaByteWriter): Unit = {
    w.putValue(obj.input, thisInfo)
  }

  def parse(r: SigmaByteReader): Values.Value[SType] = {
    val p = r.getValue().asSigmaProp
    SigmaPropBytes(p)
  }
}
