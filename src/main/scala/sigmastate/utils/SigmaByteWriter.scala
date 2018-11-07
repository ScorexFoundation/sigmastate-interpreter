package sigmastate.utils

import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.{ConstantStore, TypeSerializer, ValueSerializer}

class SigmaByteWriter(b: ByteArrayBuilder,
                      val constantExtractionStore: Option[ConstantStore]) extends ByteArrayWriter(b) {


  @inline override def put(x: Byte): SigmaByteWriter = { super.put(x); this }
  @inline override def putUByte(x: Int): SigmaByteWriter = { super.putUByte(x); this }
  @inline override def putBoolean(x: Boolean): SigmaByteWriter = { super.putBoolean(x); this }
  @inline override def putShort(x: Short): SigmaByteWriter = { super.putShort(x); this }
  @inline override def putUShort(x: Int): SigmaByteWriter = { super.putUShort(x); this }
  @inline override def putInt(x: Int): SigmaByteWriter = { super.putInt(x); this }
  @inline override def putUInt(x: Long): SigmaByteWriter = { super.putUInt(x); this }
  @inline override def putLong(x: Long): SigmaByteWriter = { super.putLong(x); this }
  @inline override def putULong(x: Long): SigmaByteWriter = { super.putULong(x); this }
  @inline override def putBytes(xs: Array[Byte]): SigmaByteWriter = { super.putBytes(xs); this }
  @inline override def putBits(xs: Array[Boolean]): SigmaByteWriter = { super.putBits(xs); this }

  @inline def putOption[T](x: Option[T])(putValue: (SigmaByteWriter, T) => Unit): SigmaByteWriter = {
    val wrap: (ByteWriter, T) => Unit = { (_, v) =>
      putValue(this, v)
    }
    super.putOption(x)(wrap); this
  }

  @inline def putType[T <: SType](x: T): SigmaByteWriter = { TypeSerializer.serialize(x, this); this }
  @inline def putValue[T <: SType](x: Value[T]): SigmaByteWriter = { ValueSerializer.serialize(x, this); this }
}
