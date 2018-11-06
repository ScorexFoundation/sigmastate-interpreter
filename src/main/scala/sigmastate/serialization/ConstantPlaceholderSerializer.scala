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
  }

  override def parseBody(r: ByteReader): Value[SType] = {
    val id = r.getUInt().toInt
    val tpe = r.payload[ConstantStore] match {
      case Some(store) => store.get(id).tpe
      case None => error("missing constant store in ByteReader.payload")
    }
    cons(id, tpe)
  }
}

