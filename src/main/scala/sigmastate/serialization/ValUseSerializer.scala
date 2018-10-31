package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.Extensions._
import sigmastate.utils.{ByteReader, ByteWriter}

case class ValUseSerializer(cons: (Int, SType) => Value[SType]) extends ValueSerializer[ValUse[SType]] {

  override val opCode: OpCode = ValUseCode

  override def serializeBody(obj: ValUse[SType], w: ByteWriter): Unit = {
    w.putUInt(obj.valId)
    w.putType(obj.tpe)
  }

  override def parseBody(r: ByteReader): Value[SType] = {
    val id = r.getUInt().toInt
    val tpe = r.getType()
    cons(id, tpe)
  }
}

