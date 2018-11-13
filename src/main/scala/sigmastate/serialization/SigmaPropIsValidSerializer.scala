package sigmastate.serialization

import sigmastate.{Values, SType}
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SigmaByteWriter, SigmaByteReader}
import sigmastate.utxo.SigmaPropIsValid
import sigmastate.utils.Extensions._

object SigmaPropIsValidSerializer extends ValueSerializer[SigmaPropIsValid] {

  override val opCode: Byte = SigmaPropIsValidCode

  def serializeBody(obj: SigmaPropIsValid, w: SigmaByteWriter): Unit = {
    w.putValue(obj.input)
  }

  def parseBody(r: SigmaByteReader): Values.Value[SType] = {
    val p = r.getValue().asSigmaProp
    SigmaPropIsValid(p)
  }
}
