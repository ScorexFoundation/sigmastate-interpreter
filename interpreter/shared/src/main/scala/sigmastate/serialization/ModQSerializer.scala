package sigmastate.serialization

import sigma.ast.SType
import sigmastate.ModQ
import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.utils.SigmaByteWriter._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

// TODO v6.0: make sure it is covered with tests (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/327)
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
