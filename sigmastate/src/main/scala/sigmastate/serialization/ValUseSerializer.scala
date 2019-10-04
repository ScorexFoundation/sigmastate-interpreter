package sigmastate.serialization

import scalan.util.Extensions.LongOps
import sigmastate.Values._
import sigmastate._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ValUseSerializer(cons: (Int, SType) => Value[SType]) extends ValueSerializer[ValUse[SType]] {
  override def opDesc = ValUse

  override def serialize(obj: ValUse[SType], w: SigmaByteWriter): Unit = {
    w.putUInt(obj.valId)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val id = r.getUInt().toIntExact  // HF change: was r.getUInt().toInt
    val tpe = r.valDefTypeStore(id)
    cons(id, tpe)
  }
}

