package sigmastate.serialization

import java.nio.ByteBuffer

import sigmastate.utils.ByteArrayBuilder
import sigmastate._

object DataSerializer {
  def serialize[T <: SType](v: T#WrappedType, tpe: T, buf: ByteArrayBuilder): Unit = tpe match {
    case SByte => buf.append(v.asInstanceOf[Byte])
    case SBoolean => buf.append(v.asInstanceOf[Boolean])
    case SInt => buf.append(v.asInstanceOf[Long])
    case tCol: SCollection[a] =>
      val arr = v.asInstanceOf[tCol.WrappedType]
      buf.append(arr.length)
      for (x <- arr)
        DataSerializer.serialize(x, tCol.elemType, buf)
    case _ => sys.error(s"Don't know how to serialize ($v, $tpe)")
  }

  def deserialize[T <: SType](tpe: T, buf: ByteBuffer): (T#WrappedType) = (tpe match {
    case SByte => buf.get()
    case SBoolean => (buf.get() != 0.toByte)
    case SInt => buf.getLong()
    case tCol: SCollection[a] =>
      val len = buf.getInt()
      val arr = (new Array(len)).asInstanceOf[SCollection[a]#WrappedType]
      for (i <- 0 until len) {
        arr(i) = deserialize(tCol.elemType, buf)
      }
      arr
    case _ => sys.error(s"Don't know how to deserialize $tpe")
  }).asInstanceOf[T#WrappedType]
}