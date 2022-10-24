package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ValUseSerializer(cons: (Int, SType) => Value[SType]) extends ValueSerializer[ValUse[SType]] {
  override def opDesc = ValUse

  override def serialize(obj: ValUse[SType], w: SigmaByteWriter): Unit = {
    w.putUInt(obj.valId)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val id = r.getUInt().toInt
    // Note, when id < 0 as a result of Int overflow, the r.valDefTypeStore(id) won't throw
    // but this will likely fail elsewhere
    val tpe = r.valDefTypeStore(id)
    cons(id, tpe)
  }
}

