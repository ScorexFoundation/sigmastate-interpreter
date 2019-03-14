package sigmastate.serialization

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.{ModQ, SType}

object ModQSerializer extends ValueSerializer[ModQ] {

  override val opCode: Byte = OpCodes.ModQCode

  def serializeBody(obj: ModQ, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "ModQ")

    SerializeLog.logPrintf(true, true, false, "input")
    w.putValue(obj.input)
    SerializeLog.logPrintf(false, true, false, "input")

    SerializeLog.logPrintf(false, true, false, "ModQ")
  }

  def parseBody(r: SigmaByteReader): Value[SType] = {
    val p = r.getValue().asBigInt
    ModQ(p)
  }
}
