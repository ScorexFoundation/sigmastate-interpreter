package sigmastate.serialization

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{ModQ, SType}

object ModQSerializer extends ValueSerializer[ModQ] {

  override val opCode: Byte = OpCodes.ModQCode

  def serializeBody(obj: ModQ, w: SigmaByteWriter): Unit = {
    w.putValue(obj.input)
  }

  def parseBody(r: SigmaByteReader): Value[SType] = {
    val p = r.getValue().asBigInt
    ModQ(p)
  }
}
