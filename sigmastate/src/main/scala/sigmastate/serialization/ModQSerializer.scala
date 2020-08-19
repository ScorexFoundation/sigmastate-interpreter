package sigmastate.serialization

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.{ModQ, SType}

// TODO HF: make sure it is covered with tests
object ModQSerializer extends ValueSerializer[ModQ] {
  override def opDesc = ModQ

  def serialize(obj: ModQ, w: SigmaByteWriter): Unit = {
    w.putValue(obj.input, "this")
  }

  def parse(r: SigmaByteReader): Value[SType] = {
    val p = r.getValue().asBigInt
    ModQ(p)
  }
}
