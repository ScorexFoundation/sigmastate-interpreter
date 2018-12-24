package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SigmaByteWriter, SigmaByteReader}

case class ValUseSerializer(cons: (Int, SType) => Value[SType]) extends ValueSerializer[ValUse[SType]] {

  override val opCode: OpCode = ValUseCode

  override def serializeBody(obj: ValUse[SType], w: SigmaByteWriter): Unit = {
    w.putUInt(obj.valId)
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val id = r.getUInt().toInt
    val tpe = r.valDefTypeStore(id)
    cons(id, tpe)
  }
}

