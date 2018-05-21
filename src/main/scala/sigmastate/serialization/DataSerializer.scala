package sigmastate.serialization
import java.nio.ByteBuffer

import sigmastate.{SInt, SType, SByte}

object DataSerializer {
  def serialize[T <: SType](v: T#WrappedType, tpe: T, buf: ByteBuffer): Unit = tpe match {
    case SByte => buf.put(v.asInstanceOf[Byte])
    case SInt => buf.putLong(v.asInstanceOf[Long])
    case _ => sys.error(s"Don't know how to serialize ($v, $tpe)")
  }

  def deserialize[T <: SType](tpe: T, buf: ByteBuffer): (T#WrappedType) = tpe match {
    case SByte => buf.get()
    case SInt => buf.getLong()
    case _ => sys.error(s"Don't know how to deserialize $tpe")
  }
}