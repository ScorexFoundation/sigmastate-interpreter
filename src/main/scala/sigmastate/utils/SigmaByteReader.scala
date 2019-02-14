package sigmastate.utils

import java.nio.ByteBuffer

import sigmastate.SType
import sigmastate.Values.SValue
import sigmastate.serialization.{ValDefTypeStore, TypeSerializer, ValueSerializer, ConstantStore}
import sigma.util.Extensions._

class SigmaByteReader(b: ByteBuffer,
                      var constantStore: ConstantStore,
                      var resolvePlaceholdersToConstants: Boolean)
  extends ByteBufferReader(b) {

  val valDefTypeStore: ValDefTypeStore = new ValDefTypeStore()

  @inline override def mark(): SigmaByteReader = {
    super.mark()
    this
  }

  @inline def getType(): SType = TypeSerializer.deserialize(this)
  @inline def getValue(): SValue = ValueSerializer.deserialize(this)
  @inline def getValues(): IndexedSeq[SValue] = {
    val size = getUInt().toIntExact
    val xs = new Array[SValue](size)
    for (i <- 0 until size) {
      xs(i) = getValue()
    }
    xs.toIndexedSeq
  }
}
