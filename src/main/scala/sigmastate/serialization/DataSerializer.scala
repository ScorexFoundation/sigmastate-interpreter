package sigmastate.serialization

import java.nio.ByteBuffer

import sigmastate.utils.ByteArrayBuilder
import sigmastate.{SInt, SType, SByte, SBoolean}

object DataSerializer {
  def serialize[T <: SType](v: T#WrappedType, tpe: T, buf: ByteArrayBuilder): Unit = tpe match {
    case SByte => buf.append(v.asInstanceOf[Byte])
    case SBoolean => buf.append(v.asInstanceOf[Boolean])
    case SInt => buf.append(v.asInstanceOf[Long])
    case _ => sys.error(s"Don't know how to serialize ($v, $tpe)")
  }

  def deserialize[T <: SType](tpe: T, buf: ByteBuffer): (T#WrappedType) = (tpe match {
    case SByte => buf.get()
    case SBoolean => (buf.get() != 0.toByte)
    case SInt => buf.getLong()
    case _ => sys.error(s"Don't know how to deserialize $tpe")
  }).asInstanceOf[T#WrappedType]
}