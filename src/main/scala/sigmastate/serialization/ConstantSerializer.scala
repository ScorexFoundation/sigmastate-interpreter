package sigmastate.serialization

import java.nio.ByteBuffer

import com.google.common.primitives.Longs
import sigmastate.{SInt, SType}
import sigmastate.SType.TypeCode
import sigmastate.Values._
import sigmastate.utils.Extensions._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.Serializer.Position

case class ConstantSerializer[T <: SType](tpe: T)
    extends ValueSerializer[Constant[T]] {
  override val opCode = IntConstantCode

  val typeCode: TypeCode = SInt.typeCode

  override def serializeBody(c: Constant[T]) = {
    val buf = ByteBuffer.allocate(10)
    DataSerializer.serialize[T](c.value, tpe, buf)
    buf.toBytes
  }

  override def parseBody(bytes: Array[Byte], pos: Position) = {
    val buf: ByteBuffer = ByteBuffer.wrap(bytes).position(pos)
    val obj = DataSerializer.deserialize[T](tpe, buf)
    Constant(obj, tpe) -> (buf.position() - pos)
  }
}

