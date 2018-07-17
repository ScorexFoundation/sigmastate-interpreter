package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values._
import sigmastate.lang.SigmaBuilder
import sigmastate.utils.Extensions._
import sigmastate.utils.{ByteReader, ByteWriter}

/** This works in tandem with DataSerializer, if you change one make sure to check the other.*/
case class ConstantSerializer(builder: SigmaBuilder)
  extends ByteBufferSerializer[Constant[SType]]  {
  override def serialize(c: Constant[SType], w: ByteWriter): Unit = {
    w.putType(c.tpe)
    DataSerializer.serialize(c.value, c.tpe, w)
  }

  override def deserialize(r: ByteReader): Constant[SType] = {
    val tpe = r.getType()
    val obj = DataSerializer.deserialize(tpe, r)
    builder.mkConstant(obj, tpe).asInstanceOf[Constant[SType]]
  }
}

