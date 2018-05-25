package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.Serializer.Position

case class ConstantSerializer[T <: SType](tpe: T)
    extends ValueSerializer[Constant[T]] {
  override val opCode = (ConstantCode + tpe.typeCode).toByte

  override def serializeBody(c: Constant[T]) = {
    val w = Serializer.startWriter()
    DataSerializer.serialize[T](c.value, tpe, w)
    w.toBytes
  }

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    val r = Serializer.startReader(bytes, pos)
    val obj = DataSerializer.deserialize[T](tpe, r)
    Constant(obj, tpe) -> (r.consumed)
  }
}

