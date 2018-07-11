package sigmastate.serialization

import sigmastate.{SType, STuple}
import sigmastate.Values._
import sigmastate.utils.{ByteWriterSigmaValues, ByteReader}

/** This works in tandem with DataSerializer, if you change one make sure to check the other.*/
object ConstantSerializer extends ByteBufferSerializer[Constant[SType]]  {
  override def serialize(c: Constant[SType], w: ByteWriterSigmaValues): Unit = {
    w.putType(c.tpe)
    DataSerializer.serialize(c.value, c.tpe, w)
  }

  override def deserialize(r: ByteReader): Constant[SType] = {
    val tpe = r.getType()
    val obj = DataSerializer.deserialize(tpe, r)
    Constant(obj, tpe)
  }
}

