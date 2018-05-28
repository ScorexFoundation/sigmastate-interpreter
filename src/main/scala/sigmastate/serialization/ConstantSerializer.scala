package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values._
import sigmastate.utils.{ByteWriter, ByteReader}

object ConstantSerializer extends ByteBufferSerializer[Constant[SType]]  {
  override def serialize(c: Constant[SType], w: ByteWriter): Unit = {
    w.putType(c.tpe)
    DataSerializer.serialize(c.value, c.tpe, w)
  }

  override def deserialize(r: ByteReader): Constant[SType] = {
    val tpe = r.getType()
    val obj = DataSerializer.deserialize(tpe, r)
    Constant(obj, tpe)
  }
}

