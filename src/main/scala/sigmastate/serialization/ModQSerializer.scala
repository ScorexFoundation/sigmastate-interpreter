package sigmastate.serialization

import sigmastate.Operations.ModQInfo
import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{ModQ, SType}

object ModQSerializer extends ValueSerializer[ModQ] {
  override def opDesc = ModQ

  def serialize(obj: ModQ, w: SigmaByteWriter): Unit = {
    w.putValue(obj.input, ModQInfo.thisArg)
  }

  def parse(r: SigmaByteReader): Value[SType] = {
    val p = r.getValue().asBigInt
    ModQ(p)
  }
}
