package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.Extensions._
import sigmastate.utils.{ByteReader, ByteWriter}

case class ConstantPlaceholderSerializer(cons: (Int, SType) => Value[SType])
  extends ValueSerializer[ConstantPlaceholder[SType]] {

  override val opCode: OpCode = ConstantPlaceholderIndexCode

  override def serializeBody(obj: ConstantPlaceholder[SType], w: ByteWriter): Unit = {
    w.putUInt(obj.id)
    w.putType(obj.tpe)
  }

  override def parseBody(r: ByteReader): Value[SType] = {
    val id = r.getUInt().toInt
    val tpe = r.getType()
    cons(id, tpe)
  }
}

