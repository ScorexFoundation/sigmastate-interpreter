package sigmastate.serialization

import java.nio.ByteBuffer

import com.google.common.primitives.Longs
import sigmastate.{SInt, SType}
import sigmastate.SType.TypeCode
import sigmastate.Values._
import sigmastate.utils.Extensions._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.Serializer.Position
import sigmastate.utils.ByteArrayBuilder

case class ConstantSerializer[T <: SType](tpe: T)
    extends ValueSerializer[Constant[T]] {
  override val opCode = (ConstantCode + tpe.typeCode).toByte

  override def serializeBody(c: Constant[T]) = {
    val b = new ByteArrayBuilder()
    DataSerializer.serialize[T](c.value, tpe, b)
    b.toBytes
  }

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    val buf = Serializer.start(bytes, pos)
    val obj = DataSerializer.deserialize[T](tpe, buf)
    Constant(obj, tpe) -> (buf.position() - pos)
  }
}

