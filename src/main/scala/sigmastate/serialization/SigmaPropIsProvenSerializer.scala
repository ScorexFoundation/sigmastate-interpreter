package sigmastate.serialization

import sigmastate.{SType, Values}
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.SigmaPropIsProven

object SigmaPropIsProvenSerializer extends ValueSerializer[SigmaPropIsProven] {

  override val opCode: Byte = SigmaPropIsProvenCode

  def serializeBody(obj: SigmaPropIsProven, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "SigmaPropIsProven")
    SerializeLog.logPrintf(true, true, false, "Input")

    w.putValue(obj.input)

    SerializeLog.logPrintf(false, true, false, "Input")
    SerializeLog.logPrintf(false, true, false, "SigmaPropIsProven")
  }

  def parseBody(r: SigmaByteReader): Values.Value[SType] = {
    val p = r.getValue().asSigmaProp
    SigmaPropIsProven(p)
  }
}
