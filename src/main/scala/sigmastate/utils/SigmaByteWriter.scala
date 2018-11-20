package sigmastate.utils

import scorex.util.ByteArrayBuilder
import scorex.util.serialization.VLQByteBufferWriter
import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.{ConstantStore, TypeSerializer, ValueSerializer}

class SigmaByteWriter(b: ByteArrayBuilder,
                      val constantExtractionStore: Option[ConstantStore]) extends VLQByteBufferWriter(b) {

  @inline def putType[T <: SType](x: T): SigmaByteWriter = { TypeSerializer.serialize(x, this); this }
  @inline def putValue[T <: SType](x: Value[T]): SigmaByteWriter = { ValueSerializer.serialize(x, this); this }
}
