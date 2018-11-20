package sigmastate.serialization

import sigmastate.{Values, SType}
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SigmaByteWriter, SigmaByteReader}
import sigmastate.utxo.SigmaPropIsValid
import scorex.util.Extensions._

object SigmaPropIsValidSerializer extends ValueSerializer[SigmaPropIsValid] {

  override val opCode: Byte = SigmaPropIsValidCode

  def serialize(obj: SigmaPropIsValid, w: SigmaByteWriter): Unit = {
    w.putValue(obj.input)
  }

  def parse(r: SigmaByteReader): Values.Value[SType] = {
    val p = r.getValue().asSigmaProp
    SigmaPropIsValid(p)
  }
}
