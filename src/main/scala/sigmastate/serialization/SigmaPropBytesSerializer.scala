package sigmastate.serialization

import sigmastate.{Values, SType}
import sigmastate.lang.Terms._
import scorex.util.Extensions._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SigmaByteWriter, SigmaByteReader}
import sigmastate.utxo.SigmaPropBytes

object SigmaPropBytesSerializer extends ValueSerializer[SigmaPropBytes] {
  override val opCode: Byte = SigmaPropBytesCode

  def serialize(obj: SigmaPropBytes, w: SigmaByteWriter): Unit = {
    w.putValue(obj.input)
  }

  def parse(r: SigmaByteReader): Values.Value[SType] = {
    val p = r.getValue().asSigmaProp
    SigmaPropBytes(p)
  }
}
