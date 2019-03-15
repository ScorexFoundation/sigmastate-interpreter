package sigmastate.serialization

import sigmastate.{SType, Values}
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.SigmaPropBytes

object SigmaPropBytesSerializer extends ValueSerializer[SigmaPropBytes] {
  override val opCode: Byte = SigmaPropBytesCode

  def serializeBody(obj: SigmaPropBytes, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "SigmaPropBytes")

    SerializeLog.logPrintf(true, true, false, "input")
    w.putValue(obj.input)
    SerializeLog.logPrintf(false, true, false, "input")

    SerializeLog.logPrintf(false, true, false, "SigmaPropBytes")
  }

  def parseBody(r: SigmaByteReader): Values.Value[SType] = {
    val p = r.getValue().asSigmaProp
    SigmaPropBytes(p)
  }
}
