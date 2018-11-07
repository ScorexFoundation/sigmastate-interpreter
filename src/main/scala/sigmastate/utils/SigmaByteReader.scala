package sigmastate.utils

import java.nio.ByteBuffer

import sigmastate.SType
import sigmastate.Values.SValue
import sigmastate.serialization.{ConstantStore, TypeSerializer, ValDefTypeStore, ValueSerializer}

class SigmaByteReader(b: ByteBuffer,
                      val constantStore: Option[ConstantStore] = None)
  extends ByteBufferReader(b) {

  val valDefTypeStore: ValDefTypeStore = new ValDefTypeStore()

  @inline override def mark(): SigmaByteReader = {
    super.mark()
    this
  }

  @inline def getType(): SType = TypeSerializer.deserialize(this)
  @inline def getValue(): SValue = ValueSerializer.deserialize(this)
}
