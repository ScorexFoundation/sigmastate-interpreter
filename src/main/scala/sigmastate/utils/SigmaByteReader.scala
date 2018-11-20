package sigmastate.utils

import java.nio.ByteBuffer

import scorex.util.serialization.VLQByteBufferReader
import sigmastate.SType
import sigmastate.Values.SValue
import sigmastate.serialization.{ConstantStore, TypeSerializer, ValDefTypeStore, ValueSerializer}

class SigmaByteReader(b: ByteBuffer,
                      val constantStore: ConstantStore,
                      val resolvePlaceholdersToConstants: Boolean)
  extends VLQByteBufferReader(b) {

  val valDefTypeStore: ValDefTypeStore = new ValDefTypeStore()

  @inline override def mark(): this.type = {
    super.mark()
    this
  }

  @inline def getType(): SType = TypeSerializer.deserialize(this)
  @inline def getValue(): SValue = ValueSerializer.deserialize(this)
}
