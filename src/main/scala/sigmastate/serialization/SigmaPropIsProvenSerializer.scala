package sigmastate.serialization

import sigmastate.{Values, SType}
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SigmaByteWriter, SigmaByteReader}
import sigmastate.utxo.SigmaPropIsProven
import scorex.util.Extensions._

object SigmaPropIsProvenSerializer extends ValueSerializer[SigmaPropIsProven] {

  override val opCode: Byte = SigmaPropIsProvenCode

  def serialize(obj: SigmaPropIsProven, w: SigmaByteWriter): Unit = {
    w.putValue(obj.input)
  }

  def parse(r: SigmaByteReader): Values.Value[SType] = {
    val p = r.getValue().asSigmaProp
    SigmaPropIsProven(p)
  }
}
