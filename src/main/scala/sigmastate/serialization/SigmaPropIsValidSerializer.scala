package sigmastate.serialization

import sigmastate.{Values, SType}
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{ByteWriter, ByteReader}
import sigmastate.utxo.SigmaPropIsValid
import sigmastate.utils.Extensions._

object SigmaPropIsValidSerializer extends ValueSerializer[SigmaPropIsValid] {

  override val opCode: Byte = SigmaPropIsValidCode

  def serializeBody(obj: SigmaPropIsValid, w: ByteWriter): Unit = {
    w.putValue(obj.input)
  }

  def parseBody(r: ByteReader): Values.Value[SType] = {
    val p = r.getValue().asSigmaProp
    SigmaPropIsValid(p)
  }
}
